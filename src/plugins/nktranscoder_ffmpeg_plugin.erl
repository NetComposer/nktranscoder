-module(nktranscoder_ffmpeg_plugin).
-export([plugin_deps/0]).
-include("nktranscoder.hrl").

plugin_deps() ->
    [nktranscoder].
