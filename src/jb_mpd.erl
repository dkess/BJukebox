-module(jb_mpd).

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
		 terminate/2]).

start_link() ->
	gen_server:start_link({local, jb_mpd}, ?MODULE, nothing, []).

init(_) ->
	{ok, Sock} = gen_tcp:connect("127.0.0.1", 6600,
								 [binary,
								  {active, true},
								  {packet, line},
								  {keepalive, true}]),
	{ok, {just_connected, Sock}}.

handle_call(_Msg, _From, S) ->
	{noreply, S, 750}.

handle_cast(Msg, {noidle, Sock}) ->
	gen_server:cast(self(), Msg),
	{noreply, {noidle, Sock}};
handle_cast({load_song, Streamurl}, {idle, Sock}) ->
	io:fwrite("loading song"),
	gen_tcp:send(Sock, [<<"noidle\n">>,
						<<"add ">>,list_to_binary(Streamurl),<<"\n">>]),
	{noreply, {added_song, Sock}};
handle_cast(skip, {idle, Sock}) ->
	io:fwrite("skipping~n"),
	gen_tcp:send(Sock, [<<"noidle\n">>,
						<<"next\n">>]),
	{noreply, {noidle, Sock}};
handle_cast(_Msg, S) ->
	{noreply, S, 750}.

handle_info({tcp, _Sock, Msg}, {Idle, Sock}) ->
	io:fwrite("~w, ~p~n", [Idle, Msg]),
	case Idle of
		just_connected ->
			case string:substr(binary_to_list(Msg), 1, 2) =:= "OK" of
				true ->
					gen_tcp:send(Sock, <<"status\n">>),
					{noreply, {check_stopped, Sock}}
			end;
		idle ->
			case binary_to_list(Msg) of
				"changed: playlist\n" ->
					gen_tcp:send(Sock, <<"status\n">>),
					{noreply, {check_stopped, Sock}};
				"OK MPD 0.19.0\n" ->
					{noreply, {idle, Sock}};
				_ ->
					{noreply, {noidle, Sock}}
			end;
		check_stopped ->
			case string:tokens(binary_to_list(Msg), " ") of
				["playlistlength:", MPDState] ->
					case MPDState of
						"0\n" ->
							io:fwrite("want song~n", []),
							gen_server:cast(manager, want_song),
							{noreply, {noidle, Sock}};
						_ ->
							{noreply, {noidle, Sock}}
					end;
				_ ->
					{noreply, {Idle, Sock}}
			end;
		added_song ->
			case binary_to_list(Msg) of
				"OK\n" ->
					io:fwrite("sending idle 1qq~n", []),
					gen_tcp:send(Sock, <<"play\n">>),
					{noreply, {noidle, Sock}}
			end;
		noidle ->
			case binary_to_list(Msg) of
				"changed: playlist\n" ->
					gen_tcp:send(Sock, <<"status\n">>),
					{noreply, {check_stopped, Sock}};
				"OK\n" ->
					io:fwrite("sending idle~n", []),
					gen_tcp:send(Sock, <<"idle\n">>),
					{noreply, {idle, Sock}};
				_ ->
					{noreply, {Idle, Sock}}
			end
	end;
handle_info(_Msg, S) ->
	{noreply, S, 750}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _S) ->
	ok.
