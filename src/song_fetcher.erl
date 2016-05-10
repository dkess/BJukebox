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
	Port = open_port({spawn_executable, os:find_executable("python3")},
					 [{line, 2000},
					  exit_status,
					  {args, ["priv/metadata.py", Songurl]}
					 ]),
	[Title, Thumbnail, Newurl] = readlines(Port),
	Boss ! {Title, Thumbnail, Newurl}.

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
			Callback ! {nomatch, error};
		Result ->
			Callback ! {match, Result}
	after 6000 ->
			  error_logger:warning_msg("youtube-dl worker ~p timed out~n", [Worker]),
			  Callback ! {nomatch, timeout}
	end.

-spec readlines(Port :: port()) -> [string()].
readlines(Port) ->
	readlines_recurse(Port, "").

-spec readlines_recurse(Port :: port(), LastText :: string()) -> [string()].
readlines_recurse(Port, LastText) ->
	receive
		{Port, {data, {eol, Data}}} ->
			[LastText ++ Data | readlines_recurse(Port, "")];
		{Port, {data, {noeol, Data}}} ->
			readlines_recurse(Port, LastText ++ Data);
		{Port, {exit_status, 0}} ->
			[];
		Unexpected ->
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
