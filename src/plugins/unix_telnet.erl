-module(unix_telnet).
-compile(export_all).

-export([connect/6,get_prompt_regexp/0]).

-define(username,"login: ").
-define(password,"Password: ").
-define(prx,"login: |Password: |\\\$ |> ").

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
%%%-----------------------------------------------------------------
%%% @hidden
%%% @spec connect(Ip,Port,Timeout,Extra) -> {ok,Handle} | {error,Reason}
%%%      Ip = string() | {integer(),integer(),integer(),integer()}
%%%      Port = integer()
%%%      Timeout = integer()
%%%      Extra = {Username,Password}
%%%      Username = string()
%%%      Password = string()
%%%      Handle = ct_telnet:handle()
%%%
%%% @doc Callback for ct_telnet.erl.
%%%
%%% <p>Setup telnet connection to a UNIX host.</p>

connect(Ip,Port,Timeout,KeepAlive,Username,Password) ->
    io:format("unix_telnet:connect~n"),
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
