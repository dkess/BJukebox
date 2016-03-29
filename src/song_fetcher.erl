-module(song_fetcher).

-export([start_link/1]).
-export([get_metadata/2, get_streamurl/2]).
-export([get_metadata_worker/2, get_streamurl_worker/2]).

% Order of songtuple: Title, Thumbnail, URL
-type songtuple() :: {string(), string(), string()}.
-export_type([songtuple/0]).

start_link(Songurl) ->
	spawn_link(?MODULE, get_youtube, [self(), Songurl]).

get_metadata(Callback, Songurl) ->
	process_flag(trap_exit, true),
	Worker = spawn_link(?MODULE, get_metadata_worker, [self(), Songurl]),
	wait_for_result(Callback, Worker).

get_metadata_worker(Boss, Songurl) ->
	error_logger:info_msg("calling youtube-dl to get metadata for ~s~n", [Songurl]),
	Port = open_port({spawn_executable, os:find_executable("youtube-dl")},
					 [{line, 2000},
					  exit_status,
					  stderr_to_stdout,
					  {args, ["--prefer-insecure",
							  "--no-playlist",
							  "--get-title",
							  "--get-thumbnail",
							  Songurl]}]),
	{Title, Thumbnail} = read_song_title(Port, ""),
	Boss ! {Title, Thumbnail, Songurl}.

get_streamurl(Callback, Songurl) ->
	process_flag(trap_exit, true),
	Worker = spawn_link(?MODULE, get_streamurl_worker, [self(), Songurl]),
	wait_for_result(Callback, Worker).

get_streamurl_worker(Boss, Songurl) ->
	error_logger:info_msg("calling youtube-dl to get streamurl for ~s~n", [Songurl]),
	Port = open_port({spawn_executable, os:find_executable("youtube-dl")},
					 [{line, 2000},
					  exit_status,
					  stderr_to_stdout,
					  {args, ["--prefer-insecure",
							  "-f", "140/http_mp3_128_url/bestaudio",
							  "--get-url",
							  "--no-playlist",
							  Songurl]}]),
	Boss ! read_streamurl(Port, "").

wait_for_result(Callback, Worker) ->
	receive
		{'EXIT', Worker, Reason} ->
			error_logger:warning_report({Worker, Reason}),
			Callback ! nomatch;
		Result ->
			Callback ! {match, Result}
	after 6000 ->
			  error_logger:warning_msg("youtube-dl worker ~p timed out~n", [Worker]),
			  Callback ! nomatch
	end.


% first line of youtube-dl metadata output: the song's title
-spec read_song_title(Port :: port(), TitleInit :: string()) -> {string(), string()}.
read_song_title(Port, TitleInit) ->
	receive
		{Port, {data, {eol, Title}}} ->
			read_song_thumbnail(Port, TitleInit ++ Title, "");
		{Port, {data, {noeol, Title}}} ->
			read_song_title(Port, TitleInit ++ Title);
		Unexpected ->
			exit({unexpected_output, Unexpected})
	end.

% second line of youtube-dl metadata output: the song's thumbnail
-spec read_song_thumbnail(Port :: port(), Title :: string(),
						  ThumbnailInit :: string()) -> {string(), string()}.
read_song_thumbnail(Port, Title, ThumbnailInit) ->
	receive
		{Port, {data, {eol, Thumbnail}}} ->
			end_get_metadata(Port, {Title, ThumbnailInit ++ Thumbnail});
		{Port, {data, {noeol, Thumbnail}}} ->
			read_song_thumbnail(Port, Title, ThumbnailInit ++ Thumbnail);
		% this gets run if the song has no thumbnail and the command exits prematurely
		{Port, {exit_status, 0}} ->
			{Title, ThumbnailInit};
		Unexpected ->
			exit({unexpected_output, Unexpected})
	end.

-spec end_get_metadata(Port :: port(), Metadata :: {string(), string()}) ->
	{string(), string()}.
% we expect a successful exit_status
end_get_metadata(Port, Metadata) ->
	receive
		{Port, {exit_status, 0}} ->
			Metadata;
		Unexpected	 ->
			exit({unexpected_output, Unexpected})
	end.

% youtube-dl output: the song's url
-spec read_streamurl(Port :: port(), UrlInit :: string()) -> string().
read_streamurl(Port, UrlInit) ->
	receive
		{Port, {data, {eol, Url}}} ->
			end_streamurl(Port, UrlInit ++ Url);
		{Port, {data, {noeol, Url}}} ->
			read_streamurl(Port, UrlInit ++ Url);
		Unexpected ->
			exit({unexpected_output, Unexpected})
	end.

-spec end_streamurl(Port :: port(), Url :: string()) -> string().
end_streamurl(Port, Url) ->
	receive
		{Port, {exit_status, 0}} ->
			Url;
		Unexpected ->
			exit({unexpected_output, Unexpected})
	end.
