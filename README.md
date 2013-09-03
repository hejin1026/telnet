telnet
======

telnet clinet for erlang


first:
{ok, TelPid} = telnet:open(TargetMod,Ip,UserName,Password).

sec:
telnet:cmd(TelPid, Cmd).

third:
telnet:close(TelPid). 