-module(jb_websocket).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-record(state, {name}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_Type, Req, _Opts) ->
    io:fwrite("Got websocket connection~n", []),
    {ok, Req, just_connected}.

websocket_handle({text, Msg}, Req, just_connected) ->
    Name = binary_to_list(Msg),
    case {validate_name(Name), gen_server:call(manager, {join, Name})} of
	{true, ok} ->
	    {ok, Req, #state{name=Name}};
	_ ->
	    {shutdown, Req, just_connected}
    end;
websocket_handle({text, Msg}, Req, State) when is_record(State, state) ->
    case string:tokens(binary_to_list(Msg), " ") of
	["queue", Songurl] ->
	    io:fwrite("sending songurl request from pid ~p~n",[self()]),
	    fetch_song_sup:query_songurl(Songurl);
	_ ->
	    nothing
    end,
    {ok, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({match, Match}, Req, State) ->
    gen_server:cast(manager, {queue, State#state.name, Match}),
    {ok, Req, State};
websocket_info({manager_state, {Current, Queues, ClientNames}}, Req, State) ->
    CurrentJson = case Current of
		      {CurrentPlayer, CurrentSong} ->
			  [<<"{\"name\":">>, string_to_bitjson(CurrentPlayer),
			   <<",\"song\":">>, songtuple_to_bitjson(CurrentSong),
			   <<"}">>];
		      noone ->
			  <<"false">>
		  end,
    {reply, [{text, [<<"{\"current\":">>, CurrentJson,
		     <<",\"queues\":">>, list_to_bitjson(fun queueentry_to_bitjson/1, Queues),
		     <<",\"clients\":">>,
		     list_to_bitjson(fun string_to_bitjson/1, ClientNames),
		     <<"}">>]}], Req, State};

websocket_info(Info, Req, State) ->
    io:fwrite("Got unknown data ~p~n", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, State) ->
    io:fwrite("Websocket client ~p disconnected.~n", [State]),
    ok.

list_to_bitjson(Func, [First | Rest]) ->
    [<<"[">>, Func(First), lists:map(fun(Term) -> [<<",">>, Func(Term)] end, Rest), <<"]">>];
list_to_bitjson(_Func, []) ->
    <<"[]">>.

queueentry_to_bitjson({Name, Songs}) ->
    [<<"{\"name\":">>, string_to_bitjson(Name), <<",\"songs\":">>, list_to_bitjson(fun songtuple_to_bitjson/1, Songs), <<"}">>].

songtuple_to_bitjson({Songname, Thumbnail, _Streamurl}) ->
    [<<"{\"title\":">>, string_to_bitjson(Songname), <<",\"thumb\":">>, string_to_bitjson(Thumbnail), <<"}">>].

string_to_bitjson(String) ->
    [<<"\"">>, re:replace(String, "[\"\\\\]", "\\\\&", [global, {return, binary}]), <<"\"">>].

validate_name(Name) ->
    % ugly if statements
    if length(Name) =:= 0 -> false;
       length(Name) > 20 -> false;
       true ->
	   case re:run(Name, "^[a-zA-Z0-9]*$") of
	       nomatch -> false;
	       _ -> true
	   end
    end.

