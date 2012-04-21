%%%贵州AC：218.201.234.228  用户名：ultranms 密码：Ultr@123
%%%----------------------------------------------------------------------
%%% Usage:
%%%    {ok,Pid}=wifioss_telnet:connect("218.201.234.228","ultranms","Ultr@123").
%%%    wifioss_telnet:cmd(Pid,"ls .").
%%%    wifioss_telnet:close(Pid).
%%%----------------------------------------------------------------------

-module(autelan_telnet).
-compile(export_all).
-export([connect/6,get_prompt_regexp/0]).

-define(username,"Gztrmccro login: ").
-define(password,"Password: ").
-define(prx,"Gztrmccro login: |Password: |\\\#|> ").

%%%-----------------------------------------------------------------
%%% @hidden
%%% @spec get_prompt_regexp() -> PromptRegexp
%%%      PromptRegexp = ct_telnet:prompt_regexp()
%%%
%%% @doc Callback for ct_telnet.erl.
%%%
%%% <p>Return the prompt regexp for telnet connections to the
%%% interwatch instrument.</p>
get_prompt_regexp() ->
    ?prx.

connect(Ip,Port,Timeout,KeepAlive,Username,Password) ->
  io:format("~p telnet:connect~n",[?MODULE]),
    Result = 
	case telnet_client:open(Ip,Port,Timeout,KeepAlive) of
            {ok,Pid} ->
		case telnet:silent_teln_expect(Pid,[],[prompt],?prx,[]) of
		    {ok,{prompt,?username},_} ->
			ok = telnet_client:send_data(Pid,Username),
			io:format("Username: ~s~n",[Username]),
			case telnet:silent_teln_expect(Pid,[],prompt,?prx,[]) of
			    {ok,{prompt,?password},_} ->
				ok = telnet_client:send_data(Pid,Password),
				Stars = lists:duplicate(length(Password),$*),
				io:format("Password: ~s~n",[Stars]),
				ok =telnet_client:send_data(Pid,""),
				case telnet:silent_teln_expect(Pid,[],prompt,
								  ?prx,[]) of
				    {ok,{prompt,Prompt},_} 
				    when Prompt=/=?username, Prompt=/=?password ->
					{ok,Pid};
				    Error ->
					io:format("Password failed\n~p\n",
						 [Error]),
					{error,Error}
				end;
			    Error ->
				io:format("Login failed\n~p\n",[Error]),
				{error,Error}
			end;
		    {ok,[{prompt,_OtherPrompt1},{prompt,_OtherPrompt2}],_} ->
			{ok,Pid};
		    Error ->
			io:format("Did not get expected prompt\n~p\n",[Error]),
			{error,Error}
		end;
	    Error ->
		io:format("Could not open telnet connection\n~p\n",[Error]),
		Error
	end,
    Result.
