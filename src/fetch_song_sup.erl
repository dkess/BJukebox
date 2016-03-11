-module(fetch_song_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([get_metadata/1, get_streamurl/1]).
-export([start_fetcher_link/3]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {#{strategy => simple_one_for_one}, [#{id => fetcher,
												start => {?MODULE, start_fetcher_link, []},
												restart => temporary,
												shutdown => brutal_kill}]}}.
start_fetcher_link(Callback, Songurl, WhatToGet) ->
	FunctionToCall = case WhatToGet of
						 metadata ->
							 get_metadata;
						 streamurl ->
							 get_streamurl
					 end,
	{ok, spawn_link(song_fetcher, FunctionToCall, [Callback, Songurl])}.

get_metadata(Songurl) ->
	supervisor:start_child(?MODULE, [self(), Songurl, metadata]).

get_streamurl(Songurl) ->
	supervisor:start_child(?MODULE, [self(), Songurl, streamurl]).
