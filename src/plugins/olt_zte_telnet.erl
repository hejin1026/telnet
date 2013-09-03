-module(olt_zte_telnet).

-author("hejin-2011-5-16").

-behavior(telnet_mod).

-export([connect/6, get_prompt_regexp/0, close/1]).

-export([backup_data/1, backup_file/1]).

-include_lib("elog/include/elog.hrl").

-define(CMD_TIMEOUT, 9000).

-define(username,"Username:").
-define(password,"Password:").
-define(termchar, "#$").
-define(page, "--More--").
-define(prx, ?page).

-define(cmd_backup_data, "show running-config").
-define(cmd_backup_file, "upload cfg startrun.dat ftp").
-define(cmd_close, "exit").

-define(splite, "\n").

get_prompt_regexp() ->
    ?prx.

get_new_prompt(Head) ->
    ?prx ++ "|" ++ Head.

close(Pid) ->
    telnet_client:send_data(Pid, ?cmd_close).

connect(Ip,Port,Timeout,KeepAlive,Username,Password) ->
    ?INFO("telnet:connect",[]),
    Result =case telnet_client:open(Ip,Port,Timeout,KeepAlive) of
                {ok,Pid} ->
                    ?INFO("open success...~p",[Pid]),
                    case telnet:silent_teln_expect(Pid,[],[prompt],?username,[]) of
                        {ok,{prompt,?username},_} ->
                            ok = telnet_client:send_data(Pid,Username),
                            ?INFO("Username: ~s",[Username]),
                            case telnet:silent_teln_expect(Pid,[],prompt,?password,[]) of
                                {ok,{prompt,?password},_} ->
                                    ok = telnet_client:send_data(Pid,Password),
                                    ?INFO("Password: ~s",[Password]),
                                    telnet_client:send_data(Pid,""),
                                    case telnet:silent_teln_expect(Pid,[],prompt, ?termchar,[]) of
                                        {ok,{prompt,Prompt},Rest}  ->
                                            ?INFO("get login over.....propmpt:~p,~p", [Prompt, Rest]),
                                            case telnet:teln_cmd(Pid, "", ?termchar, ?CMD_TIMEOUT) of
                                                {ok, Data, PromptType, Rest} ->
                                                    ?INFO("get head data .....propmpt:~p,~p", [Data, PromptType]),
                                                    NewPrx = get_new_prompt(lists:last(Data)),
                                                    {ok, Pid, NewPrx};
                                                Error ->
                                                    {error,Error}
                                            end;
                                        Error ->
                                            ?WARNING("Password failed\n~p\n",[Error]),
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

%% for data
backup_data(Telnet) ->
    {ok, Data} = get_data(Telnet, ?cmd_backup_data),
    ?INFO("get data :~p", [Data]),
    {ok, Data}.

%% for file
backup_file(Pid) ->
     get_data(Pid, ?cmd_backup_file).

get_data(Pid, Cmd) ->
    get_data(Pid, Cmd, []).

get_data(Pid, Cmd, Acc) ->
    case telnet:cmd(Pid, Cmd, ?CMD_TIMEOUT) of
        {ok, Data, ?page, _Rest} ->
            ?INFO("more: ~p", [Data]),
            Data1 =  string:join(Data, ?splite),
            get_data(Pid, " ", [Data1|Acc]);
        {ok, Data, PromptType, _Rest} ->
            ?INFO("get end data: ~p, PromptType : ~p", [Data, PromptType]),
            Data1 =  string:join(Data, ?splite),
            AllData = string:join(lists:reverse([Data1|Acc]), ?splite),
            {ok, AllData};
        Error ->
            ?WARNING("Return error: ~p", [Error]),
            Data1 = io_lib:format("telnet send cmd error, cmd: ~p, reason:~p", [Cmd, Error]),
            AllData = string:join(lists:reverse([Data1|Acc]), ?splite),
            {ok, AllData}
    end.




