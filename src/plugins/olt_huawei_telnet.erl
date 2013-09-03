-module(olt_huawei_telnet).

-author("hejin 2011-8-17").

-behavior(telnet_mod).

-export([connect/6, get_prompt_regexp/0, close/1]).

-export([backup_data/1, backup_file/1]).

-include_lib("elog/include/elog.hrl").

-define(CMD_TIMEOUT, 9000).

-define(username, "User name:").
-define(password, "User password:").
-define(termchar, "#$|>$|---- More.* ----").
-define(comfirm, "section<K>\\|").
-define(page, "---- More.* ----").
-define(logout, "Are you sure to log out").
-define(prx, ?comfirm ++ "|" ++ ?page ++ "|" ++ ?logout).

-define(keepalive, true).

-define(splite, "\n").

-define(cmd_enable, "enable").
-define(cmd_config, "display current-configuration").
-define(cmd_close, "quit").

get_prompt_regexp() ->
    ?prx.

get_new_prompt(Head) ->
    ?prx ++ "|" ++ Head.

close(Pid) ->
	telnet:teln_cmd(Pid, ?cmd_close, ?logout, ?CMD_TIMEOUT),
    telnet_client:send_data(Pid, "y").

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
                                            {error,"Password failed"}
                                    end;
                                Error ->
                                    ?WARNING("Login failed\n~p\n",[Error]),
                                    {error,"Login failed"}
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


%%%%%%%% i am a splite line  %%%%%%%%

%% for data
backup_data(Pid) ->
    telnet:cmd(Pid, ?cmd_enable),
    get_data(Pid, ?cmd_config).


%% for file
backup_file(Pid) ->
     get_data(Pid, ?cmd_enable),
     {H, MM, _S} = time(),
     Cmd1 = lists:concat(["backup configuration ftp 136.6.162.72 ", extbif:strfdate(date()),H,MM,"_config.txt"]),
     telnet:cmd(Pid, Cmd1),
     Cmd2 = lists:concat(["backup data ftp 136.6.162.72 ", extbif:strfdate(date()),H,MM, "_data.txt"]),
     telnet:cmd(Pid, Cmd2).


%%
get_data(Pid, Cmd) ->
    get_data(Pid, Cmd, []).

get_data(Pid, Cmd, Acc) ->
    case telnet:cmd(Pid, Cmd, ?CMD_TIMEOUT) of
        {ok, Data, "section<K>|" , Rest} ->
            ?INFO("get more :~p",[Rest]),
            Data1 = string:join(Data, ?splite),
            get_data(Pid, " ", [Data1|Acc]);
        {ok, [Firstline|Data], "---- More" ++ _ , Rest} ->
            ?INFO("more: ~p, First: ~p, ~n, Rest : ~p", [Data, Firstline, Rest]),
            Data1 =  string:join([Firstline|Data], ?splite),
            get_data(Pid, " ", [Data1|Acc]);
        {ok, [Firstline|Data], PromptType, "#"++_} ->
            ?INFO("get end data: ~p, PromptType : ~p, ~n", [Data, PromptType]),
            Data1 =  string:join([Firstline|Data], ?splite),
            AllData = string:join(lists:reverse([Data1|Acc]), ?splite),
            {ok, AllData};
        {ok, [Firstline|Data], PromptType, Rest} ->
            ?INFO("have prompt, First: ~p, PromptType:~p, ~n, Rest : ~p", [Firstline, PromptType, Rest]),
            Data1 =  string:join([Firstline|Data], ?splite),
            get_data(Pid, " ", [Data1|Acc]);
        Error ->
            ?WARNING("Return error: ~p", [Error]),
            Data1 = io_lib:format("telnet send cmd error, cmd: ~p, reason:~p", [Cmd, Error]),
            AllData = string:join(lists:reverse([Data1|Acc]), ?splite),
            {ok, AllData}
    end.


