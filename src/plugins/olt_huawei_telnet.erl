-module(olt_huawei_telnet).

-author("hejin 2011-8-17").

-export([connect/6, config/2,
        get_prompt_regexp/0, get_new_prompt/1,
        close/2
        ]).

-include_lib("elog/include/elog.hrl").

-define(CONN_TIMEOUT, 10000).
-define(CMD_TIMEOUT, 9000).

-define(username, "User name:").
-define(password, "User password:").
-define(termchar, "#$|>$|---- More.* ----").
-define(comfirm, "section<K>\\|").
-define(page, "---- More.* ----").
-define(logout, "Are you sure to log out").
-define(prx, ?comfirm ++ "|" ++ ?page ++ "|" ++ ?logout).

-define(splite, "\n").

-define(cmd_enable, "enable").
-define(cmd_config, "display current-configuration").
-define(cmd_close, "quit").

get_prompt_regexp() ->
    ?prx.

get_new_prompt(Head) ->
    ?prx ++ "|" ++ Head .

close(Pid, Head) ->
    ?INFO("close telnet....~p",[Head]),
    get_data(Pid, ?cmd_close, Head),
    telnet_client:send_data(Pid, "y"),
    telnet_client:close(Pid).

connect(Ip,Port,Timeout,KeepAlive,Username,Password) ->
    ?INFO("telnet:connect",[]),
    Result =case telnet_client:open(Ip,Port,Timeout,KeepAlive) of
                {ok,Pid} ->
                    ?INFO("open success...~p",[Pid]),
                    case telnet:silent_teln_expect(Pid,[],[prompt],?username,[]) of
                        {ok,{prompt,?username},_} ->
                            ok = telnet_client:send_data(Pid,Username),
                            ?INFO("~p: ~s",[?username,Username]),
                            case telnet:silent_teln_expect(Pid,[],prompt,?password,[]) of
                                {ok,{prompt,?password},_} ->
                                    ok = telnet_client:send_data(Pid,Password),
                                    ?INFO("~p: ~s",[?password,Password]),
                                    ok = telnet_client:send_data(Pid,""),
                                    case telnet:silent_teln_expect(Pid,[],prompt, ?termchar,[]) of
                                        {ok,{prompt,Prompt}, R}  ->
                                            ?INFO("get login over.....propmpt:~p,~p", [Prompt, R]),
                                            get_date_until_name(Pid);
                                        Error ->
                                            ?WARNING("Password failed\n~p\n", [Error]),
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

get_date_until_name(Pid) ->
    case telnet:teln_cmd(Pid, "", ?termchar, ?CMD_TIMEOUT) of
        {ok, _Data, "---- More" ++ _ , Rest} ->
            ?INFO("get more :~p",[Rest]),
            get_date_until_name(Pid);
        {ok, Data, PromptType, Rest} ->
            ?INFO("get head data .....propmpt:~p,~p, rest:~p", [Data, PromptType, Rest]),
            NewPrx = get_new_prompt(lists:last(Data) -- PromptType),
            {ok, Pid, NewPrx};
        Error ->
            ?ERROR("error after login :~p", [Error]),
            {error,Error}
    end.

%flow
config(Pid, Prx) ->
    get_data(Pid, ?cmd_enable, Prx),
    {ok, Data} = get_data(Pid, ?cmd_config, Prx),
    ?INFO("get data :~p", [Data]),
    {ok, Data}.

get_data(Pid, Cmd, NewPrx) ->
    get_data(Pid, Cmd, NewPrx, []).

get_data(Pid, Cmd, NewPrx, Acc) ->
    ?INFO("newprx ; ~p", [NewPrx]),
    case telnet:teln_cmd(Pid, Cmd, NewPrx, ?CMD_TIMEOUT) of
        {ok, Data, "section<K>|" , Rest} ->
            ?INFO("get more :~p",[Rest]),
            Data1 = string:join(Data, ?splite),
            get_data(Pid, " ", NewPrx, [Data1|Acc]);
        {ok, [Firstline|Data], "---- More" ++ _ , Rest} ->
            ?INFO("more: ~p, First: ~p, ~n, Rest : ~p", [Data, Firstline, Rest]),
            Data1 =  string:join([check_line(Firstline)|Data], ?splite),
            Data2 = Data1 ++ check_line(Rest),
            ?INFO("check_line: ~p, chec_rest : ~p", [Data, check_line(Rest)]),
            get_data(Pid, " ", NewPrx, [Data2|Acc]);
        {ok, [Firstline|Data], PromptType, Rest} ->
            ?INFO("get end data: ~p, PromptType : ~p, ~n, Rest :~p", [Data, PromptType, Rest]),
            Data1 =  string:join([check_line(Firstline)|Data], ?splite),
            AllData = string:join(lists:reverse([Data1|Acc]), ?splite),
            {ok, AllData};
        Error ->
            ?WARNING("Return error: ~p", [Error]),
            Data1 = io_lib:format("telnet send cmd error, cmd: ~p, reason:~p", [Cmd, Error]),
            AllData = string:join(lists:reverse([Data1|Acc]), ?splite),
            {ok, AllData}
    end.

check_line(String) ->
    check_line(String,[]).
check_line([$\r|Rest],Line) ->
    check_line(Rest,Line);
check_line("---- More ( Press 'Q' to break ) ----" ++ Data, Line) ->
    check_line(Data, Line);
check_line([$\e] ++"[37D                                     "++ [$\e] ++ "[37D" ++ [$\e] ++ "[1A" ++ Data, Line) ->
    check_line(Data, Line);
check_line([$\e] ++"[37D" ++ [$\e] ++ "[1A" ++ Data, Line) ->
    check_line(Data, Line);
check_line([$\e] ++"[37D" ++ Data, Line) ->
    check_line(Data, Line);
check_line([Char|Rest],Line) ->
    check_line(Rest,[Char|Line]);
check_line([],Line) ->
    lists:reverse(Line).
