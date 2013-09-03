-module(telnet_mod).

-author("hejin 2013-8-29").

-ifdef(use_specs).

-callback connect(string(),integer(),integer(),boolean(),string(),string()) -> 
	{ok, pid()} | {ok, pid(), string()} | 
	{error, any()}.

-callback get_prompt_regexp() -> string().

-callback close(pid()) -> ok.

-else.

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{connect, 6}, {get_prompt_regexp, 0}, {close, 1}];
behaviour_info(_Other) -> undefined.	

-endif.