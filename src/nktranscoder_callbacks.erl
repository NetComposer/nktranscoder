-module(nktranscoder_callbacks).
-export([nktranscoder_transcode/3]).
-include("nktranscoder.hrl").

-spec nktranscoder_transcode(nkservice:id(), nktranscoder:transcoder(), nkfile:file()) ->
    ok | {error, term()}.

nktranscoder_transcode(_SrvId, _Transcoder, _File) ->
    {error, invalid_transcoder}.
