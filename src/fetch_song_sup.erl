-module(fetch_song_sup).

-behavior(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([query_songurl/1]).
-export([start_fetcher_link/2]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {#{strategy => simple_one_for_one}, [#{id => fetcher,
												start => {?MODULE, start_fetcher_link, []},
												restart => temporary,
												shutdown => brutal_kill}]}}.
start_fetcher_link(Callback, Songurl) ->
	{ok, spawn_link(song_fetcher, get_youtube, [Callback, Songurl])}.

query_songurl(Songurl) ->
	io:fwrite("queried songurl ~p~n", [Songurl]),
	supervisor:start_child(?MODULE, [self(), Songurl]).
