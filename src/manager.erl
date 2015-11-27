-module(manager).

-behavior(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
		 terminate/2]).

-record(state, {queues :: [{nonempty_string(), [song_fetcher:songtuple()]}],
				current :: {nonempty_string(), song_fetcher:songtuple()} | noone,
				want_song :: boolean(),
				clients :: [nonempty_string()]}).

start_link() ->
	gen_server:start_link({local, manager}, ?MODULE, nothing, []).

init(_) ->
	{ok, #state{queues=[], current=noone, want_song=false, clients=[]}}.

handle_call({join, Name}, {FromPid, _Tag}, S) ->
	case lists:keymember(Name, 1, S#state.clients) of
		true ->
			{reply, {error, nametaken}, S};
		false ->
			gen_server:cast(self(), announce_state),
			monitor(process, FromPid),
			{reply, ok, S#state{clients=[{Name, FromPid} | S#state.clients]}}
	end;
handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(want_song, S) ->
	gen_server:cast(self(), announce_state),
	case S#state.queues of
		[] ->
			{noreply, S#state{want_song=true, current=noone}};
		[{NextPlayer, [NextSongtuple | OtherSongs]} | Rest] ->
			gen_server:cast(jb_mpd, {load_song, element(3, NextSongtuple)}),
			ToAppend = case OtherSongs of
						   [] ->
							   [];
						   _ ->
							   [{NextPlayer, OtherSongs}]
					   end,
			{noreply, S#state{want_song=false,
							  queues=lists:append(Rest, ToAppend),
							  current={NextPlayer, NextSongtuple}}}
	end;

handle_cast({queue, Name, Songtuple}, S) ->
	gen_server:cast(self(), announce_state),
	case lists:keyfind(Name, 1, S#state.queues) of
		{Name, Songlist} ->
			{noreply,
			 S#state{queues=lists:keyreplace(Name,
											 1,
											 S#state.queues,
											 {Name, lists:append(Songlist,
																 [Songtuple])})}};
		false ->
			case S#state.queues of
				[] ->
					case S#state.want_song of
						true ->
							gen_server:cast(self(), want_song);
						_ ->
							nothing
					end,
					{noreply, S#state{queues=[{Name, [Songtuple]}]}};
				[{TopPlayer, _}]  when TopPlayer =:= element(1, S#state.current) ->
					{noreply, S#state{queues=[{Name, [Songtuple]} | S#state.queues]}};
				_ ->
					{noreply, S#state{queues=lists:append(S#state.queues,
														  [{Name, [Songtuple]}])}}
			end
	end;
handle_cast(announce_state, S) ->
	ToSend = {S#state.current,
			  S#state.queues,
			  lists:map(fun({Name, _Pid}) -> Name end, S#state.clients)},
	lists:map(fun({_Name, Pid}) -> Pid ! {manager_state, ToSend} end, S#state.clients),
	{noreply, S};
handle_cast(_Msg, S) ->
	{noreply, S, 750}.

% client has disconnected
handle_info({'DOWN', _Ref, process, Pid, _Reason}, S) ->
	io:fwrite("client ~p disconnected~n", [Pid]),
	{noreply, S#state{clients=lists:keydelete(Pid, 2, S#state.clients)}};
handle_info(_Msg, S) ->
	{noreply, S, 750}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _S) ->
	ok.
