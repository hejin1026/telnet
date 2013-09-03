-module(sw_telnet).

-author("hejin 2013-8-23").

-behavior(telnet_mod).

-export([connect/6, close/1, get_prompt_regexp/0]).

-include_lib("elog/include/elog.hrl").

-define(CMD_TIMEOUT, 9000).

-define(username, "H3C login:").
-define(password, "Password:").
-define(termchar, ">").

-define(keepalive, true).

-define(splite, "\n").

-define(cmd_close, "quit\r").
-define(cmd_newline, "\r").

get_prompt_regexp() ->
    ?termchar.

close(Pid) ->
	telnet_client:send_data(Pid,?cmd_close).

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
    case telnet:teln_cmd(Pid, ?cmd_newline, ?termchar, ?CMD_TIMEOUT) of
        {ok, Data, PromptType, Rest} ->
            ?INFO("get head data .....propmpt:~p,~p, rest:~p", [Data, PromptType, Rest]),
            {ok, Pid, lists:last(Data)};
        Error ->
            ?ERROR("error after login :~p", [Error]),
            {error,Error}
    end.


