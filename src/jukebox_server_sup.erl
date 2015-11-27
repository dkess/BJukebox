%%%-------------------------------------------------------------------
%% @doc jukebox_server top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('jukebox_server_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%MaMa% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_one, 0, 1},
	  [#{id=>jb_mpd, start => {jb_mpd, start_link, []}},
	   #{id=>manager, start => {manager, start_link, []}},
	   #{id=>fetch_song_sup,
	     start => {fetch_song_sup, start_link, []},
	     type => supervisor}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
