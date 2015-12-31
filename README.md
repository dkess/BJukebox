# BJukebox

BJukebox is a web interface for contributing songs to a live playlist. Songs can come from Youtube, Soundcloud, or any website the youtube-dl program supports.

The server uses Erlang to manage websockets with clients, and an MPD server as audio output. 

[Screenshot!](/../screenshots/screenshot.png?raw=true)

Build
-----
    $ rebar3 compile

Dependencies
-----
* [youtube-dl](https://github.com/rg3/youtube-dl/)
* An [MPD server](http://www.musicpd.org/) running on port 6600, with consume ON, repeat and shuffle OFF
* The [cowboy](https://github.com/ninenines/cowboy) library (should be automatically downloaded by rebar3)
