-module(song_fetcher).

-export([start_link/1]).
-export([get_youtube/2]).

% Order of songtuple: Title, Thumbnail, URL
-type songtuple() :: {string(), string(), string()}.
-export_type([songtuple/0]).

start_link(Songurl) ->
	spawn_link(?MODULE, get_youtube, [self(), Songurl]).

-spec get_youtube(Callback :: pid(), Songurl :: string()) ->
	nomatch | {match, songtuple()}.
get_youtube(Callback, Songurl) ->
	io:fwrite("get youtube called~n", []),
	Port = open_port({spawn_executable, os:find_executable("youtube-dl")},
					 [{line, 2000},
					  exit_status,
					  {args, ["--prefer-insecure",
							 "-f", "140/http_mp3_128_url/bestaudio",
							 "-eg",
							 "--no-playlist",
							 "--get-thumbnail",
							 Songurl]}]),
	Callback ! read_song_title(Port, "").



% first line of youtube-dl output: the song's title
-spec read_song_title(Port :: port(), TitleInit :: string()) -> songtuple().
read_song_title(Port, TitleInit) ->
	receive
		{Port, {data, {eol, Title}}} ->
			read_song_url(Port, TitleInit ++ Title, "");
		{Port, {data, {noeol, Title}}} ->
			read_song_title(Port, TitleInit ++ Title);
		{Port, {exit_status, _}} ->
			nomatch;
		_ ->
			read_song_title(Port, TitleInit)
	end.

% second line of youtube-dl output: the song's url
-spec read_song_url(Port :: port(), Title :: string(),
					UrlInit :: string()) -> songtuple().
read_song_url(Port, Title, UrlInit) ->
	receive
		{Port, {data, {eol, Url}}} ->
			read_song_thumbnail(Port, Title, UrlInit ++ Url, "");
		{Port, {data, {noeol, Url}}} ->
			read_song_url(Port, Title, UrlInit ++ Url);
		{Port, {exit_status, _}} ->
			nomatch;
		_ ->
			read_song_url(Port, Title, UrlInit)
	end.

% third line of youtube-dl output: the song's thumbnail
-spec read_song_thumbnail(Port :: port(), Title :: string(), Url :: string(),
						  ThumbnailInit :: string()) -> songtuple().
read_song_thumbnail(Port, Title, Url, ThumbnailInit) ->
	receive
		{Port, {data, {eol, Thumbnail}}} ->
			end_youtube_dl(Port, {Title, ThumbnailInit ++ Thumbnail, Url});
		{Port, {data, {noeol, Thumbnail}}} ->
			read_song_thumbnail(Port, Title, Url, ThumbnailInit ++ Thumbnail);
		{Port, {exit_status, _}} ->
			nomatch;
		_ ->
			read_song_thumbnail(Port, Title, Url, ThumbnailInit)
	end.

-spec end_youtube_dl(Port :: port(), Songtuple :: songtuple()) -> songtuple().
% we expect a successful exit_status
end_youtube_dl(Port, Songtuple) ->
	receive
		{Port, {exit_status, 0}} ->
			{match, Songtuple};
		{Port, _} ->
			nomatch;
		_ ->
			end_youtube_dl(Port, Songtuple)
	end.
