%%%-------------------------------------------------------------------
%% @doc jukebox_server public API
%% @end
%%%-------------------------------------------------------------------

-module('jukebox_server_app').

-behaviour(application).

%% Application callbacks
-export([start/2
	 ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/", cowboy_static, {priv_file, jukebox_server, "index.html"}},
					     {"/ws", jb_websocket, []},
					     {"/static/[...]", cowboy_static, {priv_dir, jukebox_server, "static"}}
					    ]}
				     ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
				[{env, [{dispatch, Dispatch}]}]),
    'jukebox_server_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
