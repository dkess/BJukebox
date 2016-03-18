-module(jb_mpd).

-behavior(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
		 terminate/2]).

-define(RESTART_WAIT_MILLIS, 4000).

start_link() ->
	gen_server:start_link({local, jb_mpd}, ?MODULE, nothing, []).

init(_) ->
	gen_server:cast(self(), connect),
	{ok, disconnected}.

handle_call(_Msg, _From, S) ->
	{noreply, S, 750}.

handle_cast(connect, disconnected) ->
	case gen_tcp:connect("127.0.0.1", 6600,
						 [binary,
						  {active, true},
						  {packet, line},
						  {keepalive, true}]) of
		{ok, Sock} ->
			error_logger:info_msg("Connected to MPD~n"),
			gen_server:cast(manager, got_conn),
			{noreply, {just_connected, Sock}};
		{error, _Reason} ->
			erlang:send_after(?RESTART_WAIT_MILLIS, self(), reconnect),
			{noreply, disconnected}
	end;
handle_cast(Msg, {noidle, Sock}) ->
	gen_server:cast(self(), Msg),
	{noreply, {noidle, Sock}};
handle_cast({load_song, Streamurl}, {idle, Sock}) ->
	error_logger:info_msg("loading song into mpd: ~s~n", [Streamurl]),
	gen_tcp:send(Sock, [<<"noidle\n">>,
						<<"add ">>,list_to_binary(Streamurl),<<"\n">>]),
	{noreply, {added_song, Sock}};
handle_cast(skip, {idle, Sock}) ->
	error_logger:info_msg("skipping song~n"),
	gen_tcp:send(Sock, [<<"noidle\n">>,
						<<"clear\n">>]),
	{noreply, {noidle, Sock}};
handle_cast({setvol, Volume}, {idle, Sock}) ->
	gen_tcp:send(Sock, [<<"noidle\n">>,
						<<"setvol ">>, integer_to_binary(round(Volume)), <<"\n">>]),
	{noreply, {noidle, Sock}};
handle_cast({voldelta, Delta}, {idle, Sock}) ->
	gen_tcp:send(Sock, [<<"noidle\n">>,
						<<"status\n">>]),
	{noreply, {{voldelta, Delta}, Sock}};
handle_cast(want_volume, {idle, Sock}) ->
	gen_tcp:send(Sock, [<<"noidle\n">>,
						<<"status\n">>]),
	{noreply, {want_volume, Sock}};
handle_cast(_Msg, S) ->
	{noreply, S, 750}.

handle_info(reconnect, S) ->
	gen_server:cast(self(), connect),
	{noreply, S};

handle_info({tcp, _Sock, Msg}, {Idle, Sock}) ->
	io:fwrite("MPD (~w) ~s", [Idle, Msg]),
	case Idle of
		just_connected ->
			case string:substr(binary_to_list(Msg), 1, 2) =:= "OK" of
				true ->
					gen_tcp:send(Sock, <<"status\n">>),
					{noreply, {check_stopped, Sock}}
			end;
		idle ->
			case binary_to_list(Msg) of
				"changed: player\n" ->
					gen_tcp:send(Sock, <<"status\n">>),
					{noreply, {check_stopped, Sock}};
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
					gen_tcp:send(Sock, <<"play\n">>),
					{noreply, {noidle, Sock}}
			end;
		found_error ->
			case binary_to_list(Msg) of
				"OK\n" ->
					gen_tcp:send(Sock, <<"clearerror\n">>),
					gen_server:cast(self(), skip),
					{noreply, {noidle, Sock}};
				_ ->
					{noreply, {Idle, Sock}}
			end;
		{voldelta, Delta} ->
			case string:tokens(binary_to_list(Msg), " ") of
				["volume:", CurrentVol] ->
					{noreply, {{newvol,
								list_to_integer(string:strip(CurrentVol, right, $\n))
								+ Delta},
							   Sock}};
				_ ->
					{noreply, {Idle, Sock}}
			end;
		{newvol, Newvol} ->
			case binary_to_list(Msg) of
				"OK\n" ->
					Vol = case Newvol of
							  V when V < 0 -> 0;
							  V when V > 100 -> 100;
							  V -> V
						  end,
					gen_tcp:send(Sock, [<<"setvol ">>,
										integer_to_binary(Vol),
										<<"\n">>]),
					gen_server:cast(manager, {volume, Vol}),
					{noreply, {noidle, Sock}};
				_ ->
					{noreply, {Idle, Sock}}
			end;
		want_volume ->
			case string:tokens(binary_to_list(Msg), " ") of
				["volume:", CurrentVol] ->
					Vol = list_to_integer(string:strip(CurrentVol, right, $\n)),
					gen_server:cast(manager, {volume, Vol}),
					{noreply, {noidle, Sock}};
				_ ->
					{noreply, {Idle, Sock}}
			end;
		noidle ->
			case binary_to_list(Msg) of
				"changed: playlist\n" ->
					gen_tcp:send(Sock, <<"status\n">>),
					{noreply, {check_stopped, Sock}};
				"OK\n" ->
					gen_tcp:send(Sock, <<"idle\n">>),
					{noreply, {idle, Sock}};
				LMsg ->
					SError = "error:",
					case string:substr(LMsg, 1, length(SError)) of
						SError ->
							{noreply, {found_error, Sock}};
						_ ->
							{noreply, {Idle, Sock}}
					end
			end
	end;
handle_info({tcp_closed,_Sock}, _S) ->
	error_logger:info_msg("Lost connection to MPD~n"),
	gen_server:cast(manager, lost_conn),
	gen_server:cast(self(), connect),
	{noreply, disconnected};
handle_info(_Msg, S) ->
	{noreply, S, 750}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _S) ->
	ok.
