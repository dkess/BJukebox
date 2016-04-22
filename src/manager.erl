-module(manager).

-behavior(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
		 terminate/2]).

-record(state, {queues :: [{nonempty_string(), [song_fetcher:songtuple()]}],
				current :: {nonempty_string(), song_fetcher:songtuple()} | noone | disconnected,
				clients :: [{nonempty_string(), pid()}],
			    loading :: loading | notloading}).

start_link() ->
	gen_server:start_link({local, manager}, ?MODULE, nothing, []).

init(_) ->
	{ok, #state{queues=[], current=disconnected, clients=[], loading=notloading}}.

handle_call({join, Name}, {FromPid, _Tag}, S) ->
	case lists:keymember(Name, 1, S#state.clients) of
		true ->
			{reply, {error, nametaken}, S};
		false ->
			gen_server:cast(jb_mpd, want_volume),
			gen_server:cast(self(), announce_state),
			monitor(process, FromPid),
			{reply, ok, S#state{clients=[{Name, FromPid} | S#state.clients]}}
	end;
handle_call(_Msg, _From, State) ->
	{noreply, State}.

handle_cast(want_song, S) ->
	error_logger:info_msg("got want_song ~p~n", [S]),
	gen_server:cast(self(), announce_state),
	case S#state.queues of
		[] ->
			{noreply, S#state{current=noone}};
		[{NextPlayer, [NextSongtuple | OtherSongs]} | Rest] ->
			fetch_song_sup:get_streamurl(element(3, NextSongtuple)),
			ToAppend = case OtherSongs of
						   [] ->
							   [];
						   _ ->
							   [{NextPlayer, OtherSongs}]
					   end,
			{noreply, S#state{queues=lists:append(Rest, ToAppend),
							  current={NextPlayer, NextSongtuple},
							  loading=loading}}
	end;

handle_cast({queue, Name, Songtuple}, S) ->
	error_logger:info_msg("~s is queueing songtuple ~p~n",[Name, Songtuple]),
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
					case S#state.current of
						noone ->
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
% Remove the element in QueuePos from Name's queue.  If name does not have a
% queue, or QueuePos does not exist, do nothing.
handle_cast({remove, Name, QueuePos}, S) ->
	gen_server:cast(self(), announce_state),
	{noreply,
	 case lists:keyfind(Name, 1, S#state.queues) of
		 {_Name, ThisQueue} ->
			 case lists:split(QueuePos, ThisQueue) of
				 % if this is the last element in the queue, remove
				 % the whole queue
				 {[], [_|[]]} ->
					 S#state{queues=lists:keydelete(Name, 1, S#state.queues)};
				 {L1, [_|L2]} ->
					 S#state{queues=lists:keyreplace(Name, 1, S#state.queues, {Name, L1 ++ L2})};
				 _ ->
					 S
			 end;
		 _ ->
			 S
	 end};
handle_cast(skipme, S) when S#state.loading == notloading ->
	gen_server:cast(jb_mpd, skip),
	{noreply, S};

handle_cast({send_to_all, Msg}, S) ->
	lists:map(fun({_Name, Pid}) -> Pid ! Msg end, S#state.clients),
	{noreply, S};

handle_cast(announce_state, S) ->
	ToSend = {S#state.current,
			  S#state.queues,
			  lists:map(fun({Name, _Pid}) -> Name end, S#state.clients)},
	gen_server:cast(self(), {send_to_all, {manager_state, ToSend}}),
	{noreply, S};

handle_cast({volume, Vol}, S) ->
	gen_server:cast(self(), {send_to_all, {announce_vol, Vol}}),
	{noreply, S};

handle_cast(lost_conn, S) ->
	gen_server:cast(self(), announce_state),
	{noreply, S#state{current = disconnected}};
handle_cast(got_conn, S) ->
	gen_server:cast(self(), announce_state),
	{noreply, S#state{current = noone}};
handle_cast(_Msg, S) ->
	{noreply, S, 750}.

% we got the streaurl of a song, now play it
handle_info({match, Streamurl}, S) ->
	gen_server:cast(jb_mpd, {load_song, Streamurl}),
	{noreply, S#state{loading=notloading}};
handle_info(nomatch, S) ->
	gen_server:cast(self(), want_song),
	{noreply, S#state{loading=notloading}};
% client has disconnected
handle_info({'DOWN', _Ref, process, Pid, _Reason}, S) ->
	gen_server:cast(self(), update_volumes),
	{noreply, S#state{clients=lists:keydelete(Pid, 2, S#state.clients)}};
handle_info(_Msg, S) ->
	{noreply, S, 750}.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

terminate(_Reason, _S) ->
	ok.
