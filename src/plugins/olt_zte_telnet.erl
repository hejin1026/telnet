-module(olt_zte_telnet).

-author("hejin-2011-5-16").

-export([start/1,
        send_data/2,
        get_data/2, get_data/3,
        close/2
        ]).

-include("elog.hrl").

-define(CONN_TIMEOUT, 10000).
-define(CMD_TIMEOUT, 9000).

-define(username,"Username:").
-define(password,"Password:").
-define(termchar, "#$").
-define(page, "--More--").
-define(prx, ?username ++ "|" ++ ?password ++ "|" ++ ?page).

-define(keepalive, true).

-define(splite, "\n").

start(Opts) ->
    init(Opts).

send_data(Pid, Data) ->
    telnet_client:send_data(Pid, Data).

get_data(Pid, Head) ->
    get_data(Pid, "show running-config", Head).

get_data(Pid, Cmd, Head) ->
    get_data(Pid, Cmd, Head, []).

get_data(Pid, Cmd, Head, Acc) ->
    NewPrx = ?prx ++ "|" ++ Head ++ "#",
    case telnet_gen_conn:teln_cmd(Pid, Cmd, NewPrx, ?CMD_TIMEOUT) of
        {ok, Data, ?page, Rest} ->
            Lastline1 = string:strip(lists:last(Data)),
            ?INFO("more: ~p, lastline: ~p, ~n, Rest : ~p", [Data, Lastline1, Rest]),
            Data1 =  string:join(Data, ?splite),
            Data2 = Data1 ++ check_line(Rest),
            get_data(Pid, " ", Head, [Data2|Acc]);
        {ok, Data, PromptType, Rest} ->
            ?INFO("get end data: ~p, PromptType : ~p, ~n, Rest :~p", [Data, PromptType, Rest]),
            Data1 =  string:join(Data, ?splite),
            Data2 = Data1 ++ Rest,
            AllData = string:join(lists:reverse([Data2|Acc]), ?splite),
            {ok, AllData};
        Error ->
            ?WARNING("Return error: ~p", [Error]),
            Data1 = io_lib:format("telnet send cmd error, cmd: ~p, reason:~p", [Cmd, Error]),
            AllData = string:join(lists:reverse([Data1|Acc]), ?splite),
            {ok, AllData}
    end.

%add hejin 2011-8-23 check rest
check_line(String) ->
    check_line(String,[]).
check_line([$\r|Rest],Line) ->
    check_line(Rest,Line);
check_line([$\b|Rest],Line) ->
    check_line(Rest,Line);
check_line([Char|Rest],Line) ->
    check_line(Rest,[Char|Line]);
check_line([],Line) ->
    lists:reverse(Line).

close(Pid, _Head) ->
    send_data(Pid, "quit"),
    telnet_client:close(Pid).

init(Opts) ->
    io:format("starting telnet conn ...~p",[Opts]),
    Host = proplists:get_value(host, Opts, "localhost"),
    Port = proplists:get_value(port, Opts, 23),
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    case (catch connect(Host, Port, ?CONN_TIMEOUT, ?keepalive, Username, Password)) of
	{ok, Pid, Head} ->
	    {ok, Pid, Head};
	{error, Error} ->
	    {stop, Error};
    {'EXIT', Reason} ->
        {stop, Reason}
	end.

connect(Ip,Port,Timeout,KeepAlive,Username,Password) ->
    ?INFO("telnet:connect",[]),
    Result =case telnet_client:open(Ip,Port,Timeout,KeepAlive) of
                {ok,Pid} ->
                    ?INFO("open success...~p",[Pid]),
                    case telnet:silent_teln_expect(Pid,[],[prompt],?prx,[]) of
                        {ok,{prompt,?username},_} ->
                            ok = telnet_client:send_data(Pid,Username),
                            ?INFO("Username: ~s",[Username]),
                            case telnet:silent_teln_expect(Pid,[],prompt,?prx,[]) of
                                {ok,{prompt,?password},_} ->
                                    ok = telnet_client:send_data(Pid,Password),
%                                   Stars = lists:duplicate(length(Password),$*),
                                    ?INFO("Password: ~s",[Password]),
%                                   ok = telnet_client:send_data(Pid,""),
                                    case telnet:silent_teln_expect(Pid,[],prompt,
                                                                   ?termchar,[]) of
                                        {ok,{prompt,Prompt},Rest}  ->
                                            ?INFO("get login over.....propmpt:~p,~p", [Prompt, Rest]),
                                            case telnet_gen_conn:teln_cmd(Pid, "", ?termchar, ?CMD_TIMEOUT) of
                                                {ok, Data, PromptType, Rest} ->
                                                    ?INFO("get head data .....propmpt:~p,~p", [Data, PromptType]),
                                                    {ok, Pid, string:join(lists:reverse(Data), "")};
                                                Error ->
                                                    {error,Error}
                                            end;
                                        Error ->
                                            ?WARNING("Password failed\n~p\n",
                                                     [Error]),
                                            {error,Error}
                                    end;
                                Error ->
                                    ?WARNING("Login failed\n~p\n",[Error]),
                                    {error,Error}
                            end;
                        {ok,[{prompt,_OtherPrompt1},{prompt,_OtherPrompt2}],_} ->
                            {ok,Pid, "."};
                        Error ->
                            ?WARNING("Did not get expected prompt\n~p\n",[Error]),
                            {error,Error}
                    end;
                Error ->
                    ?WARNING("Could not open telnet connection\n~p\n",[Error]),
                    {error, Error}
            end,
    Result.
