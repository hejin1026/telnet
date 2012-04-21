-module(telnet_gen_conn).
-include("telnet.hrl").
-include("elog.hrl").
%%TODO gen_server ?
%% Callbacks
-export([init/5,init_gen/6,handle_msg/2,reconnect/1,terminate/2]).

-export([teln_receive_until_prompt/3, teln_cmd/4, do_within_time/2]).

init_gen(Parent,Ip,Port,UserName,Password,S0) ->
    process_flag(trap_exit,true),
    put(silent,false),
    case catch init(Ip,Port,UserName,Password,S0) of
        {ok,ConnPid,State} when is_pid(ConnPid) ->
            link(ConnPid),
            put(conn_pid,ConnPid),
            Parent ! {connected,self()},
            loop(State);
        {error,Reason} ->
            Parent ! {{error,Reason},self()}
    end.

init(Ip,Port,UserName,Password,S0) ->
    TargetMod = S0#state.target_mod,
    case catch TargetMod:connect(Ip,Port,S0#state.conn_to,S0#state.keep_alive,UserName,Password) of
        {ok,TelnPid} ->
            ?INFO(
                "Opened telnet connection\n"
                "IP: ~p\n"
                "Port: ~p\n"
                "Command timeout: ~p\n"
                "Reconnection attempts: ~p\n"
                "Reconnection interval: ~p\n"
                "Connection timeout: ~p\n"
                "Keep alive: ~w",
                [Ip,Port,S0#state.com_to,S0#state.reconns,
                 S0#state.reconn_int,S0#state.conn_to,S0#state.keep_alive]),

            {ok,TelnPid,S0#state{teln_pid=TelnPid,
                                 type=ip,
                                 target_mod=TargetMod,
                                 extra={Ip,Port,UserName,Password},
                                 prx=TargetMod:get_prompt_regexp()}};
        {'EXIT',Reason} ->
            {error,Reason};
        Error ->
            Error
    end.

loop(State) ->
    receive
        {'EXIT',Pid,Reason} when Pid == State#state.teln_pid ->
            ?INFO("Connection down!\nOpening new!,Reason: ~p\nAddress: ~p\n",
                  [Reason,State#state.extra]),
            case reconnect(State) of
                {ok, NewPid, NewState} ->
                    link(NewPid),
                    put(conn_pid,NewPid),
                    loop(NewState);
                Error ->
                    % ct_util:unregister_connection(self()),
                    ?INFO("Reconnect failed. Giving up!,Reason: ~p\n",[Error])
            end;
        {'EXIT',_Pid,_Reason} ->
            %	     exit(Reason),
            loop(State);
        {stop, From} ->
            terminate(State#state.teln_pid,State),
            return(From,ok),
            ok;
        {Msg,From={Pid,_Ref}} when is_pid(Pid) ->
            {Return,NewState} = handle_msg(Msg,State),
            return(From, Return),
            loop(NewState);
        {Msg} ->
            ?INFO("good msg ~p~n",[Msg])
    end.

reconnect(State=#state{extra={Ip,Port,_,_}}) ->
    reconnect(Ip,Port,State#state.reconns,State).

reconnect(Ip,Port,N,State=#state{target_mod=TargetMod,
                                 keep_alive=KeepAlive,
                                 extra={_,_,UserName,Password},
                                 conn_to=ConnTo,
                                 reconn_int=ReconnInt}) ->
    case TargetMod:connect(Ip,Port,ConnTo,KeepAlive,UserName,Password) of
        {ok, NewPid} ->
            {ok, NewPid, State#state{teln_pid=NewPid}};
        Error when N==0 ->
            Error;
        _Error ->
            ?INFO("Reconnect failed!,Retries left: ~p",[N]),
            timer:sleep(ReconnInt),
            reconnect(Ip,Port,N-1,State)
    end.

handle_msg({cmd,Cmd,Timeout},State) ->
    case {State#state.type,State#state.prompt} of
        {ts,_} ->
            telnet:silent_teln_expect(State#state.teln_pid,
                                      State#state.buffer,
                                      prompt,
                                      State#state.prx,
                                      [{timeout,2000}]);
        {ip,false} ->
            telnet:silent_teln_expect(State#state.teln_pid,
                                      State#state.buffer,
                                      prompt,
                                      State#state.prx,
                                      [{timeout,200}]);
        {ip,true} ->
            ok
    end,

    TO = if Timeout == default -> State#state.com_to;
             true -> Timeout
         end,

    {Return,NewBuffer,Prompt} = 
        case teln_cmd(State#state.teln_pid, Cmd, State#state.prx, TO) of
            {ok,Data,_PromptType,Rest} ->
                ?INFO("Return: ~p", [{ok,Data}]),
                {{ok,Data},Rest,true};
            Error ->
                Retry = {retry,{Error,State#state.name,State#state.teln_pid, {cmd,Cmd,TO}}},
                ?INFO("Return: ~p", [Error]),
                {Retry,[],false}
        end,
    {Return,State#state{buffer=NewBuffer,prompt=Prompt}};

handle_msg({send,Cmd},State) ->
    ?INFO("Cmd: ~p",[Cmd]),
    case {State#state.type,State#state.prompt} of
	{ts,_} -> 
          telnet:silent_teln_expect(State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{timeout,2000}]);
	{ip,false} -> 
          telnet:silent_teln_expect(State#state.teln_pid,
			       State#state.buffer,
			       prompt,
			       State#state.prx,
			       [{timeout,200}]);
	{ip,true} ->
	    ok
    end,
    telnet_client:send_data(State#state.teln_pid,Cmd),
    {ok,State#state{buffer=[],prompt=false}};

handle_msg(get_data,State) ->
    {ok,Data,Buffer} = telnet:teln_get_all_data(State#state.teln_pid,
					 State#state.prx,
					 State#state.buffer,
					 [],[]),
    ?INFO("Return: ~p",[{ok,Data}]),
    {{ok,Data},State#state{buffer=Buffer}}.

terminate(TelnPid,_State) ->
    ?INFO("Closing telnet connection.\nId: ~p",[TelnPid]),
    telnet_client:close(TelnPid).

return({To,Ref},Result) ->
    To ! {Ref, Result}.

teln_cmd(Pid,Cmd,Prx,Timeout) ->
    telnet_client:send_data(Pid,Cmd),
    teln_receive_until_prompt(Pid,Prx,Timeout).

teln_receive_until_prompt(Pid,Prx,Timeout) ->
    Fun = fun() -> teln_receive_until_prompt(Pid,Prx,[],[]) end,
    do_within_time(Fun, Timeout).

teln_receive_until_prompt(Pid,Prx,Acc,LastLine) ->
    {ok,Data} = telnet_client:get_data(Pid),
    ?INFO("get data :~p, ~n, ~p", [Data, LastLine]),
    case check_for_prompt(Prx,LastLine ++ Data) of
        {prompt,Lines,PromptType,Rest} ->
            ?INFO("get prompt :~p, ~p, ~p", [Lines, PromptType, Rest]),
            Return = lists:reverse(lists:append([Lines|Acc])),
            {ok,Return,PromptType,Rest};
        {noprompt,Lines,LastLine1} ->
            ?INFO("get noprompt :~p, ~p", [Lines, LastLine1]),
            teln_receive_until_prompt(Pid,Prx,[Lines|Acc],LastLine1)
    end.

check_for_prompt(Prx,Data) ->
    case match_prompt(Data,Prx) of
        {prompt,UptoPrompt,PromptType,Rest} ->
            {RevLines,LastLine} = split_lines(UptoPrompt),
            {prompt,[LastLine|RevLines],PromptType,Rest};
        noprompt ->
            {RevLines,Rest} = split_lines(Data),
            {noprompt,RevLines,Rest}
    end.

split_lines(String) ->
    split_lines(String,[],[]).
split_lines([$\n|Rest],Line,Lines) ->
    split_lines(Rest,[],[lists:reverse(Line)|Lines]);
split_lines([$\r|Rest],Line,Lines) ->
    split_lines(Rest,Line,Lines);
%add hejin 2011-5-20 写文件，操作符无意义
split_lines([$\b|Rest],Line,Lines) ->
    split_lines(Rest,Line,Lines);
split_lines([0|Rest],Line,Lines) ->
    split_lines(Rest,Line,Lines);
split_lines([Char|Rest],Line,Lines) ->
    split_lines(Rest,[Char|Line],Lines);
split_lines([],Line,Lines) ->
    {Lines,lists:reverse(Line)}.


match_prompt(Str,Prx) ->
    match_prompt(Str,Prx,[]).
match_prompt(Str,Prx,Acc) ->
    case re:run(Str,Prx) of
	nomatch ->
	    noprompt;
	{match,[{Start,Len}]} ->
	    case split_prompt_string(Str,Start+1,Start+Len,1,[],[]) of
		{noprompt,Done,Rest} ->
		    match_prompt(Rest,Prx,Done);
		{prompt,UptoPrompt,Prompt,Rest} ->
		    {prompt,lists:reverse(UptoPrompt++Acc),
		     lists:reverse(Prompt),Rest}
	    end
    end.

split_prompt_string([Ch|Str],Start,End,N,UptoPrompt,Prompt) when N<Start ->
    split_prompt_string(Str,Start,End,N+1,[Ch|UptoPrompt],Prompt);
split_prompt_string([Ch|Str],Start,End,N,UptoPrompt,Prompt) when N>=Start, N<End->
    split_prompt_string(Str,Start,End,N+1,UptoPrompt,[Ch|Prompt]);
split_prompt_string([Ch|Rest],_Start,End,N,UptoPrompt,Prompt) when N==End ->
    case UptoPrompt of
        [$",$=,$T,$P,$M,$O,$R,$P|_] ->
            %% This is a line from "listenv", it is not a real prompt
            {noprompt,[Ch|Prompt]++UptoPrompt,Rest};
        [$\s,$t,$s,$a|_] when Prompt==":nigol" ->
            %% This is probably the "Last login:" statement which is
            %% written when telnet connection is openend.
            {noprompt,[Ch|Prompt]++UptoPrompt,Rest};
        _ ->
            %get data  no need prompt, hejin 2011-5-20
            %{prompt,[Ch|Prompt]++UptoPrompt,[Ch|Prompt],Rest}
            {prompt,UptoPrompt,[Ch|Prompt],Rest}
    end.

do_within_time(Fun,Timeout) ->
    Self = self(),
    Silent = get(silent),
    TmpPid = spawn_link(fun() -> put(silent,Silent),
                                R = Fun(),
                                Self ! {self(),R}
                        end),
    ConnPid = get(conn_pid),
    receive 
        {TmpPid,Result} ->
            Result;
        {'EXIT',ConnPid,_Reason}=M ->
            unlink(TmpPid),
            exit(TmpPid,kill),
            self() ! M,
            {error,connection_closed}
    after 
        Timeout ->
            ?INFO("time out ...~p", [Timeout]),
            exit(TmpPid,kill),
            receive
                {TmpPid,Result} ->
                    %% TmpPid just managed to send the result at the same time
                    %% as the timeout expired.
                    receive {'EXIT',TmpPid,_reason} -> ok end,
                    Result;
                {'EXIT',TmpPid,killed} ->
                    %% TmpPid did not send the result before the timeout expired.
                    {error,timeout}
            end
    end.


