%%%----------------------------------------------------------------------
%%% File    : elog.hrl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : Elog header
%%% Created : 31 Mar 2009
%%% License : http://www.opengoss.com/
%%%
%%% Copyright (C) 2007-2009, www.opengoss.com 
%%%----------------------------------------------------------------------
-define(ELOG_VERSION, "2.0").

%% ---------------------------------
%% Logging mechanism

-define(PRINT(Format, Args),
    io:format(Format, Args)).

-define(DEBUG(Format, Args),
    elog_logger:debug_msg(?MODULE,?LINE,Format, Args)).

-define(INFO(Format, Args),
    elog_logger:info_msg(?MODULE,?LINE,Format, Args)).
			      
-define(WARNING(Format, Args),
    elog_logger:warning_msg(?MODULE,?LINE,Format, Args)).
			      
-define(ERROR(Format, Args),
    elog_logger:error_msg(?MODULE,?LINE,Format, Args)).

-define(CRITICAL(Format, Args),
    elog_logger:critical_msg(?MODULE,?LINE,Format, Args)).

