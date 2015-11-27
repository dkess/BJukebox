# BJukebox

BJukebox is a web interface for contributing songs to a live playlist. Songs can come from Youtube, Soundcloud, or any website the youtube-dl program supports.

The server uses Erlang to manage websockets with clients, and an MPD server as audio output. 

Build
-----
    $ rebar3 compile

Dependencies
-----
* youtube-dl
* An MPD server running on port 6600, with consume ON, repeat and shuffle OFF
* The cowboy library (should be automatically downloaded by rebar3)
