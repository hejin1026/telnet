
-module(telnet).

-create("hejin 2013-1-5").

-compile(export_all).

-export([open/3, open/4, cmd/2, config/1, send/2, close/1]).

%% Callbacks
-export([init/3,handle_msg/2,reconnect/2,terminate/2]).

%% Tool internals
-export([silent_teln_expect/5, teln_receive_until_prompt/3, teln_cmd/4]).

-include_lib("elog/include/elog.hrl").

-define(DEFAULT_PORT,23).
-define(DEFAULT_TIMEOUT,10000).

-record(state,{teln_pid,
	       prx,
	       buffer=[],
	       prompt=false,
	       name,
	       keep_alive=true,
	       extra,
	       target_mod,
	       conn_to=?DEFAULT_TIMEOUT,
	       com_to=?DEFAULT_TIMEOUT
       }).

open(TargetMod,Ip,UserName,Password) ->
    open(TargetMod,Ip,?DEFAULT_PORT,UserName,Password).

open(TargetMod,Ip,Port,UserName,Password) ->
    Self = self(),
    Pid  = spawn(fun() -> init(Self,Ip,Port,UserName,Password,#state{target_mod=TargetMod}) end),
    MRef = erlang:monitor(process,Pid),
    receive
        {connected,Pid} ->
            erlang:demonitor(MRef, [flush]),
            {ok,Pid};
        {Error,Pid} ->
            receive {'DOWN',MRef,process,_,_} -> ok end,
		    Error;
        {'DOWN',MRef,process,_,Reason} ->
		    ?INFO("monet_telnet:Connection process died: ~p\n",[Reason]),
		    {error,{connection_process_died,Reason}}
    end.
    
cmd(Pid,Cmd) ->
    call(Pid,{cmd,Cmd,default}).

config(Pid) ->
    call(Pid,config).

send(TelnPid,Cmd) ->
	call(TelnPid,{send,Cmd,default}).

close(Pid) ->
    call(Pid,stop).

call(Pid,Msg) ->
    MRef = erlang:monitor(process,Pid),
    Ref = make_ref(),
    Pid ! {Msg,{self(),Ref}},
    receive
        {Ref, Result} ->
            erlang:demonitor(MRef, [flush]),
            case Result of
                {retry,_Data} ->
                    call(Pid,Result);
                Other ->
                    Other
            end;
        {'DOWN',MRef,process,_,Reason}  ->
            {error,{process_down,Pid,Reason}}
    end.

%% Callbacks
init(Parent,Ip,Port,UserName,Password,State) ->
    process_flag(trap_exit,true),
    case catch connect(Ip,Port,UserName,Password,State) of
        {ok,ConnPid,State} when is_pid(ConnPid) ->
            link(ConnPid),
            put(conn_pid,ConnPid),
            Parent ! {connected,self()},
            loop(State);
        {error,Reason} ->
            Parent ! {{error,Reason},self()}
    end.

connect(Ip,Port,UserName,Password,S0) ->
    TargetMod = S0#state.target_mod,
    case catch TargetMod:connect(Ip,Port,S0#state.conn_to,S0#state.keep_alive,UserName,Password) of
        {ok,TelnPid} ->
            State = S0#state{teln_pid=TelnPid,
                                 target_mod=TargetMod,
                                 extra={Ip,Port,UserName,Password},
                                 prx=TargetMod:get_prompt_regexp()},
            ?INFO("telnpid: ~p, state:~p",[TelnPid,State]),
            {ok,TelnPid,State};
        {ok, TelnPid, NewPrx} ->
            State = S0#state{teln_pid=TelnPid,
                                 target_mod=TargetMod,
                                 extra={Ip,Port,UserName,Password},
                                 prx=NewPrx},
            ?INFO("telnpid: ~p, state:~p",[TelnPid,State]),
            {ok,TelnPid,State};
        {'EXIT',Reason} ->
            {error,Reason};
        Error ->
            Error
    end.

reconnect(State=#state{target_mod=TargetMod,
				 keep_alive=KeepAlive,
				 extra={Ip,Port,UserName,Password},
				 conn_to=ConnTo}) ->
    case TargetMod:connect(Ip,Port,ConnTo,KeepAlive,UserName,Password) of
        {ok, NewPid} ->
            {ok, NewPid, State#state{teln_pid=NewPid}};
        Error ->
            ?INFO("Reconnect failed: ~p",[Error]),
            Error
    end.

loop(State) ->
    receive
        {'EXIT',Pid,Reason} when Pid == State#state.teln_pid ->
            ?ERROR("Connection down!\nOpening new!,Reason: ~p\nAddress: ~p\n", [Reason,State#state.extra]),
            case reconnect(State) of
                {ok, NewPid, NewState} ->
                    link(NewPid),
                    put(conn_pid,NewPid),
                    loop(NewState);
                Error ->
                    ?INFO("Reconnect failed. Giving up!","Reason: ~p\n",[Error])
            end;
        {'EXIT',_Pid,_Reason} ->
            loop(State);
        {stop, From} ->
            terminate(State#state.teln_pid,State),
            return(From,ok),
            ok;
        {{retry,{Error,_Name,CPid,_Msg}}, From} when CPid == State#state.conn_pid ->
            %% only retry if failure is because of a reconnection
            Return = case Error of
                 {error,_} -> Error;
                 Reason -> {error,Reason}
                 end,
            return(From, Return),
            loop(State);
        {{retry,{_Error,_Name,_CPid,Msg}}, From} ->
            ?ERROR("Rerunning command","Connection reestablished. Rerunning command...",[]),
            {Return,NewState} = handle_msg(Msg,State),
            return(From, Return),
            loop(NewState);
        {Msg,From={Pid,_Ref}} when is_pid(Pid) ->
            {Return,NewState} = handle_msg(Msg,State),
            return(From, Return),
            loop(NewState)
    end.

handle_msg(config,State) ->
    case State#state.target_mod:config() of
        {ok, NewPid} ->
            {ok, NewPid, State#state{teln_pid=NewPid}};
        Error ->
            ?INFO("Reconnect failed: ~p",[Error]),
            Error
    end;

handle_msg({cmd,Cmd,Timeout},State) ->
    case {State#state.type,State#state.prompt} of
        {ip,false} ->
            silent_teln_expect(State#state.teln_pid,
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
            {ok,Data,PromptType,Rest} ->
                ?INFO("Return: ~p", [{ok,Data}]),
                {{ok,Data,PromptType},Rest,true};
            Error ->
                Retry = {retry,{Error,State#state.name,State#state.teln_pid, {cmd,Cmd,TO}}},
                ?INFO("Return: ~p", [Error]),
                {Retry,[],false}
        end,
    {Return,State#state{buffer=NewBuffer,prompt=Prompt}};

handle_msg({send,Cmd},State) ->
    ?INFO("Cmd: ~p",[Cmd]),
    case {State#state.type,State#state.prompt} of
        {ip,false} ->
            silent_teln_expect(State#state.teln_pid,
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
    {ok,Data,Buffer} = teln_get_all_data(State#state.teln_pid,
					 State#state.prx,
					 State#state.buffer,
					 [],[]),
    ?INFO("Return: ~p",[{ok,Data}]),
    {{ok,Data},State#state{buffer=Buffer}}.

terminate(TelnPid,State) ->
    ?INFO("Closing telnet connection.\nId: ~p",[TelnPid]),
    State#state.target_mod:close(TelnPid).

return({To,Ref},Result) ->
    To ! {Ref, Result}.

%%%=================================================================
%%% Internal function

%%%=================================================================
%%% Abstraction layer on top of ct_telnet_client.erl
teln_cmd(Pid,Cmd,Prx,Timeout) ->
    telnet_client:send_data(Pid,Cmd),
    teln_receive_until_prompt(Pid,Prx,Timeout).

teln_get_all_data(Pid,Prx,Data,Acc,LastLine) ->
    case check_for_prompt(Prx,lists:reverse(LastLine) ++ Data) of
	{prompt,Lines,_PromptType,Rest} ->
	    teln_get_all_data(Pid,Prx,Rest,[Lines|Acc],[]);
	{noprompt,Lines,LastLine1} ->
	    case telnet_client:get_data(Pid) of
		{ok,[]} ->
		    {ok,lists:reverse(lists:append([Lines|Acc])),
		     lists:reverse(LastLine1)};
		{ok,Data1} ->
		    teln_get_all_data(Pid,Prx,Data1,[Lines|Acc],LastLine1)
	    end
    end.
    
%% Expect options record
-record(eo,{teln_pid,
	    prx,
	    timeout,
	    haltpatterns=[],
	    seq=false,
	    repeat=false,
	    found_prompt=false}).

%% @hidden
%% @doc Externally the silent_teln_expect function shall only be used
%% by the TargetModule, i.e. the target specific module which
%% implements connect/2 and get_prompt_regexp/0.
silent_teln_expect(Pid,Data,Pattern,Prx,Opts) ->
    ?INFO("silent_teln_expect/5, Pattern = ~p",[Pattern]),
    Result = teln_expect(Pid,Data,Pattern,Prx,Opts),
    ?INFO("silent_teln_expect -> ~p\n",[Result]),
    Result.

%% teln_expect/5 
%%
%% This function implements the expect functionality over telnet. In
%% general there are three possible ways to go:
%% 1) Single: One or more patterns are given, and the function return
%% when one of the patterns are matched.
%% 2) Sequence: Several patterns are given, and they are matched in
%% the order they appear in the pattern list.
%% 3a) Repeat (single): 1) is repeated either N times or until a halt
%% condition is fullfilled.
%% 3b) Repeat (sequence): 2) is repeated either N times or until a
%% halt condition is fullfilled.
teln_expect(Pid,Data,Pattern0,Prx,Opts) ->
    HaltPatterns = case get_ignore_prompt(Opts) of
        true -> get_haltpatterns(Opts);
        false-> [prompt | get_haltpatterns(Opts)]
    end,

    Seq = get_seq(Opts),
    Pattern = convert_pattern(Pattern0,Seq),

    Timeout = get_timeout(Opts),
 
    EO = #eo{teln_pid=Pid,
	     prx=Prx,
	     timeout=Timeout,
	     seq=Seq,
	     haltpatterns=HaltPatterns},
    ?INFO("eo:~p",[EO]),
    
    case get_repeat(Opts) of
	false ->
	    case teln_expect1(Data,Pattern,[],EO) of
		{ok,Matched,Rest} ->
		    {ok,Matched,Rest};
		{halt,Why,Rest} ->
		    {error,Why,Rest};
		{error,Reason} ->
		    {error,Reason}
	    end;
	N ->
	    EO1 = EO#eo{repeat=N},
	    repeat_expect(Data,Pattern,[],EO1)
    end.

convert_pattern(Pattern,Seq) 
  when is_list(Pattern) and not is_integer(hd(Pattern)) ->
    case Seq of
	true -> Pattern;
	false -> rm_dupl(Pattern,[])
    end;
convert_pattern(Pattern,_Seq) ->
    [Pattern].

rm_dupl([P|Ps],Acc) ->
    case lists:member(P,Acc) of
	true ->
	    rm_dupl(Ps,Acc);
	false ->
	    rm_dupl(Ps,[P|Acc])
    end;
rm_dupl([],Acc) ->
    lists:reverse(Acc).

get_timeout(Opts) ->
    case lists:keysearch(timeout,1,Opts) of
	{value,{timeout,T}} -> T;
	false -> ?DEFAULT_TIMEOUT
    end.
get_repeat(Opts) ->
    case lists:keysearch(repeat,1,Opts) of
	{value,{repeat,N}} when is_integer(N) ->
	    N;
	false ->
	    case lists:member(repeat,Opts) of
		true ->
		    -1;
		false ->
		    false
	    end
    end.
get_seq(Opts) ->
    lists:member(sequence,Opts).
get_haltpatterns(Opts) ->
    case lists:keysearch(halt,1,Opts) of
	{value,{halt,HaltPatterns}} ->
	    convert_pattern(HaltPatterns,false);
	false ->
	    []
    end.
get_ignore_prompt(Opts) ->    
    lists:member(ignore_prompt,Opts).
	
%% Repeat either single or sequence. All match results are accumulated
%% and returned when a halt condition is fulllfilled.
repeat_expect(Rest,_Pattern,Acc,#eo{repeat=0}) ->
    {ok,lists:reverse(Acc),done,Rest};
repeat_expect(Data,Pattern,Acc,EO) ->
    case teln_expect1(Data,Pattern,[],EO) of
	{ok,Matched,Rest} ->
	    EO1 = EO#eo{repeat=EO#eo.repeat-1},
	    repeat_expect(Rest,Pattern,[Matched|Acc],EO1);
	{halt,Why,Rest} ->
	    {ok,lists:reverse(Acc),Why,Rest};
	{error,Reason} ->
	    {error,Reason}
    end.

teln_expect1(Data,Pattern,Acc,EO) ->
    ExpectFun = case EO#eo.seq of
		    true -> fun() -> seq_expect(Data,Pattern,Acc,EO) end;
		    false -> fun() -> one_expect(Data,Pattern,EO) end
		end,
    case ExpectFun() of
	{match,Match,Rest} ->
        ?INFO("expect:~p, ~p",[Match, Rest]),
	    {ok,Match,Rest};
	{halt,Why,Rest} ->
	    {halt,Why,Rest};
	NotFinished ->
	    %% Get more data
        ?INFO("expect:~p",[NotFinished]),
	    Fun = fun() -> get_data1(EO#eo.teln_pid) end,
	    case do_within_time(Fun, EO#eo.timeout) of
		{error,Reason} -> 
		    %% A timeout will occur when the telnet connection
		    %% is idle for EO#eo.timeout milliseconds.
		    {error,Reason};
		{ok,Data1} ->
            ?INFO("gen_conn:~p", [Data1]),
		    case NotFinished of
			{nomatch,Rest} ->
			    %% One expect
			    teln_expect1(Rest++Data1,Pattern,[],EO);
			{continue,Patterns1,Acc1,Rest} ->
			    %% Sequence
			    teln_expect1(Rest++Data1,Patterns1,Acc1,EO)
		    end
	    end
    end.

get_data1(Pid) ->
    case telnet_client:get_data(Pid) of
	{ok,[]} ->
	    get_data1(Pid);
	{ok,Data} ->
	    {ok,Data}
    end.

%% 1) Single expect.
%% First the whole data chunk is searched for a prompt (to avoid doing
%% a regexp match for the prompt at each line).
%% If we are searching for anyting else, the datachunk is split into
%% lines and each line is matched against each pattern.

%% one_expect: split data chunk at prompts
one_expect(Data,Pattern,EO) ->
    ?INFO("prompt:~p",[match_prompt(Data,EO#eo.prx)]),
    case match_prompt(Data,EO#eo.prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    case Pattern of 
		[Prompt] when Prompt==prompt; Prompt=={prompt,PromptType} ->
		    %% Only searching for prompt
		    log_lines(UptoPrompt),
		    ?INFO("<b>PROMPT:</b> ~s", [PromptType]),
		    {match,{prompt,PromptType},Rest};
		[{prompt,_OtherPromptType}] ->
		    %% Only searching for one specific prompt, not thisone
		    log_lines(UptoPrompt),
		    {nomatch,Rest};
		_ ->
		    one_expect1(UptoPrompt,Pattern,Rest,
				EO#eo{found_prompt=PromptType})
	    end;
	noprompt ->
	    case Pattern of
		[Prompt] when Prompt==prompt; element(1,Prompt)==prompt ->
		    %% Only searching for prompt
		    LastLine = log_lines_not_last(Data),
            ?INFO("get lastline:~p", [LastLine]),
		    {nomatch,LastLine};
		_ ->
		    one_expect1(Data,Pattern,[],EO#eo{found_prompt=false})
	    end
    end.

remove_zero(List) ->
    [Ch || Ch <- List, Ch=/=0, Ch=/=13].

%% one_expect1: split data chunk at lines
one_expect1(Data,Pattern,Rest,EO) ->
    case match_lines(Data,Pattern,EO) of
	{match,Match,MatchRest} ->
	    {match,Match,MatchRest++Rest};
	{nomatch,prompt} ->
	    one_expect(Rest,Pattern,EO);
	{nomatch,NoMatchRest} ->
	    {nomatch,NoMatchRest++Rest};
	{halt,Why,HaltRest} ->
	    {halt,Why,HaltRest++Rest}
    end.
    

%% 2) Sequence.
%% First the whole data chunk is searched for a prompt (to avoid doing
%% a regexp match for the prompt at each line).
%% If we are searching for anyting else, the datachunk is split into
%% lines and each line is matched against the first pattern in the list.
%% When a match is found, the match result is accumulated, and we keep
%% searching for the next pattern in the list.

%% seq_expect: Split data chunk at prompts
seq_expect(Data,[],Acc,_EO) ->
    {match,lists:reverse(Acc),Data};
seq_expect([],Patterns,Acc,_EO) ->
    {continue,Patterns,lists:reverse(Acc),[]};
seq_expect(Data,Patterns,Acc,EO) ->
    case match_prompt(Data,EO#eo.prx) of
	{prompt,UptoPrompt,PromptType,Rest} ->
	    seq_expect1(UptoPrompt,Patterns,Acc,Rest,
			EO#eo{found_prompt=PromptType});
	noprompt ->
	    seq_expect1(Data,Patterns,Acc,[],EO#eo{found_prompt=false})
    end.

%% seq_expect1: For one prompt-chunk, match each pattern - line by
%% line if it is other than the prompt we are seaching for.
seq_expect1(Data,[prompt|Patterns],Acc,Rest,EO) ->
    case EO#eo.found_prompt of
	false ->
	    LastLine = log_lines_not_last(Data),
	    %% Rest==[] because no prompt is found
	    {continue,[prompt|Patterns],Acc,LastLine};
	PromptType ->
	    log_lines(Data),
	    %%try_cont_log("<b>PROMPT:</b> ~s", [PromptType]),
	    seq_expect(Rest,Patterns,[{prompt,PromptType}|Acc],EO)
    end;
seq_expect1(Data,[{prompt,PromptType}|Patterns],Acc,Rest,EO) ->
    case EO#eo.found_prompt of
	false ->
	    LastLine = log_lines_not_last(Data),
	    %% Rest==[] because no prompt is found
	    {continue,[{prompt,PromptType}|Patterns],Acc,LastLine};
	PromptType ->
	    log_lines(Data),
	    %%try_cont_log("<b>PROMPT:</b> ~s", [PromptType]),
	    seq_expect(Rest,Patterns,[{prompt,PromptType}|Acc],EO);
	_OtherPromptType ->
	    log_lines(Data),
	    seq_expect(Rest,[{prompt,PromptType}|Patterns],Acc,EO)
    end;
seq_expect1(Data,[Pattern|Patterns],Acc,Rest,EO) ->
    case match_lines(Data,[Pattern],EO) of
	{match,Match,MatchRest} ->
	    seq_expect1(MatchRest,Patterns,[Match|Acc],Rest,EO);
	{nomatch,prompt} ->
	    seq_expect(Rest,[Pattern|Patterns],Acc,EO);
	{nomatch,NoMatchRest} when Rest==[] ->
	    %% The data did not end with a prompt
	    {continue,[Pattern|Patterns],Acc,NoMatchRest};
	{halt,Why,HaltRest} ->
	    {halt,Why,HaltRest++Rest}
    end;
seq_expect1(Data,[],Acc,Rest,_EO) ->
    {match,lists:reverse(Acc),Data++Rest}.

%% Split prompt-chunk at lines
match_lines(Data,Patterns,EO) ->
    FoundPrompt = EO#eo.found_prompt,
    case one_line(Data,[]) of
	{noline,Rest} when FoundPrompt=/=false ->
	    %% This is the line including the prompt
	    case match_line(Rest,Patterns,FoundPrompt,EO) of
		nomatch ->
		    {nomatch,prompt};
		{Tag,Match} ->
		    {Tag,Match,[]}
	    end;
	{noline,Rest} ->
	    {nomatch,Rest};
	{Line,Rest} ->
	    case match_line(Line,Patterns,false,EO) of
		nomatch ->
		    match_lines(Rest,Patterns,EO);
		{Tag,Match} ->
		    {Tag,Match,Rest}
	    end
    end.
    

%% For one line, match each pattern
match_line(Line,Patterns,FoundPrompt,EO) ->
    match_line(Line,Patterns,FoundPrompt,EO,match).

match_line(Line,[prompt|Patterns],false,EO,RetTag) ->
    match_line(Line,Patterns,false,EO,RetTag);
match_line(Line,[prompt|_Patterns],FoundPrompt,_EO,RetTag) ->
    ?INFO("       ~s", [Line]),
    ?INFO("<b>PROMPT:</b> ~s", [FoundPrompt]),
    {RetTag,{prompt,FoundPrompt}};
match_line(Line,[{prompt,PromptType}|_Patterns],FoundPrompt,_EO,RetTag) 
  when PromptType==FoundPrompt ->
    ?INFO("       ~s", [Line]),
    ?INFO("<b>PROMPT:</b> ~s", [FoundPrompt]),
    {RetTag,{prompt,FoundPrompt}};
match_line(Line,[{prompt,PromptType}|Patterns],FoundPrompt,EO,RetTag) 
  when PromptType=/=FoundPrompt ->
    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
match_line(Line,[{Tag,Pattern}|Patterns],FoundPrompt,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list}]) of
	nomatch ->
	    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
	{match,Match} ->
	    %%try_cont_log("<b>MATCH:</b> ~s", [Line]),
	    {RetTag,{Tag,Match}}
    end;
match_line(Line,[Pattern|Patterns],FoundPrompt,EO,RetTag) ->
    case re:run(Line,Pattern,[{capture,all,list}]) of
	nomatch ->
	    match_line(Line,Patterns,FoundPrompt,EO,RetTag);
	{match,Match} ->
	    %%try_cont_log("<b>MATCH:</b> ~s", [Line]),
	    {RetTag,Match}
    end;
match_line(Line,[],FoundPrompt,EO,match) ->
    match_line(Line,EO#eo.haltpatterns,FoundPrompt,EO,halt);
match_line(Line,[],_FoundPrompt,_EO,halt) ->
    %%try_cont_log("       ~s", [Line]),
    nomatch.

one_line([$\n|Rest],Line) ->
    {lists:reverse(Line),Rest};
one_line([$\r|Rest],Line) ->
    one_line(Rest,Line);
one_line([0|Rest],Line) ->
    one_line(Rest,Line);
one_line([Char|Rest],Line) ->
    one_line(Rest,[Char|Line]);
one_line([],Line) ->
    {noline,lists:reverse(Line)}.

debug_log_lines(String) ->
    Old = put(silent,true),
    log_lines(String),
    put(silent,Old).

log_lines(String) ->
    case log_lines_not_last(String) of
	[] ->
	    ok;
	LastLine ->
	    ?INFO("       ~s", [LastLine])
    end.

log_lines_not_last(String) ->
    ?INFO("get string :~p",[String]),
    case add_tabs(String,[],[]) of
	{[],LastLine} ->
	    LastLine;
	{String1,LastLine} ->
	    ?INFO("~s",[String1]),
        LastLine
    end.

add_tabs([0|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,LastLine);
add_tabs([$\r|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,LastLine);
add_tabs([$\n|Rest],Acc,LastLine) ->
    add_tabs(Rest,[$\n|LastLine] ++ [$\s,$\s,$\s,$\s,$\s,$\s,$\s|Acc],[]);
add_tabs([Ch|Rest],Acc,LastLine) ->
    add_tabs(Rest,Acc,[Ch|LastLine]);
add_tabs([],[$\n|Acc],LastLine) ->
    {lists:reverse(Acc),lists:reverse(LastLine)};
add_tabs([],[],LastLine) ->
    {[],lists:reverse(LastLine)}.




%%% @hidden
teln_receive_until_prompt(Pid,Prx,Timeout) ->
    Fun = fun() -> teln_receive_until_prompt(Pid,Prx,[],[]) end,
    do_within_time(Fun, Timeout).

teln_receive_until_prompt(Pid,Prx,Acc,LastLine) ->
    {ok,Data} = telnet_client:get_data(Pid),
    case check_for_prompt(Prx,LastLine ++ Data) of
	{prompt,Lines,PromptType,Rest} ->
	    Return = lists:reverse(lists:append([Lines|Acc])),
	   {ok,Return,PromptType,Rest};
	{noprompt,Lines,LastLine1} ->
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
split_prompt_string([Ch|Str],Start,End,N,UptoPrompt,Prompt) 
  when N>=Start, N<End->
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
	    {prompt,[Ch|Prompt]++UptoPrompt,[Ch|Prompt],Rest}
    end.

%%%-----------------------------------------------------------------
%%% @spec do_within_time(Fun,Timeout) -> FunResult | {error,Reason}
%%%      Fun = function()
%%%      Timeout = integer()
%%%
%%% @doc Execute a function within a limited time (tool-internal use only).
%%%
%%% <p>Execute the given <code>Fun</code>, but interrupt if it takes
%%% more than <code>Timeout</code> milliseconds.</p>
%%%
%%% <p>The execution is also interrupted if the connection is
%%% closed.</p>
do_within_time(Fun,Timeout) ->
    Self = self(),
    TmpPid = spawn_link(fun() ->
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