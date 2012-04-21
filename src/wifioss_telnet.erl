%%%----------------------------------------------------------------------
%%% Usage:
%%%    {ok,Pid}=wifioss_telnet:connect(zte_olt_telnet,"192.168.1.31","opengoss","public").
%%%    wifioss_telnet:cmd(Pid,"ls .").
%%%    wifioss_telnet:close(Pid).
%%%----------------------------------------------------------------------

-module(wifioss_telnet).
-include("telnet.hrl").
-include("elog.hrl").
-export([connect/3, connect/4, connect/5,close/1]).
-export([cmd/2, cmd/3]).
    %, cmdf/3, cmdf/4, get_data/1, send/2, sendf/3, expect/2, expect/3]).

close(Pid) ->
	case stop(Pid) of
		{error,{process_down,Pid,noproc}} ->
		    {error,already_closed};
		Result ->
		    Result
	end.

stop(Pid) ->
    call(Pid,stop).

cmd(Pid,Cmd) ->
    cmd(Pid,Cmd,default).

cmd(Pid,Cmd,Timeout) ->
  call(Pid,{cmd,Cmd,Timeout}).

connect(Ip,UserName,Password) ->
   connect(?DEFAULT_Mod,Ip,?DEFAULT_PORT,UserName,Password).

connect(TargetMod,Ip,UserName,Password) ->
   connect(TargetMod,Ip,?DEFAULT_PORT,UserName,Password).

connect(TargetMod,Ip,Port,UserName,Password) ->
    Self = self(),
    Pid  = spawn(fun() -> 
    telnet_gen_conn:init_gen(Self,Ip,Port,UserName,Password,#state{target_mod=TargetMod})
           end),
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
