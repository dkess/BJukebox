# BJukebox

BJukebox is a web interface for contributing songs to a live playlist. Songs can come from Youtube, Soundcloud, or any website the youtube-dl program supports.

The server uses Erlang to manage websockets with clients, and an MPD server as audio output. 

[Screenshot!](/../screenshots/screenshot.png?raw=true)

Running
-----
1. Start your mpd server. Run `mpc consume on`, `mpc repeat off`, `mpc random off`, `mpc single off` and clear the playlist.
1. `rebar3 release` to compile. Download the rebar3 binary [here](https://www.rebar3.org/). You might need to install some extra Erlang dependencies on your server.
2. Run `_build/default/rel/jukebox_server/bin/jukebox_server start` to start the webserver.


Dependencies
-----
* [youtube-dl](https://github.com/rg3/youtube-dl/) should be installed on the webserver.
* An [MPD server](http://www.musicpd.org/) running on localhost port 6600 (SSH reverse tunnels are OK), with consume ON, repeat and shuffle OFF
* The [cowboy](https://github.com/ninenines/cowboy) library (should be automatically downloaded by rebar3)
