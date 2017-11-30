-module(nktranscoder_callbacks).
-export([nktranscoder_transcode/3, nktranscoder_parse_transcoder/2]).
-include("nktranscoder.hrl").

nktranscoder_transcode(_SrvId, _Transcoder, _Req) ->
    {error, please_implement_me}.

nktranscoder_parse_transcoder(_Config, _Opts) ->
    {error, please_implement_me}.

