-module(nktranscoder_ffmpeg_callbacks).
-export([plugin_deps/0, nktranscoder_transcode/3]).
-export([transcoder_disconnected/1,
         transcoder_connected/1,
         transcoder_error/2,
         transcoder_invalid/2,
         transcoder_finished/2,
         transcoder_progress/2]).
-include("nktranscoder.hrl").

plugin_deps() -> 
    [nktranscoder].

nktranscoder_transcode(_SrvId, #{ class := ffmpeg }=Transcoder, _File) ->
    InputType = s3,
    OutputType = s3,
    InputPath = <<"some/file">>,
    OutputPath = <<"some/other/file">>,
    Mime = <<"video/x-flv">>,
    Req = #{ input => #{ type => InputType,
                         path => InputPath },
             output => #{ type => OutputType,
                          path => OutputPath },
             content_type => Mime},
    {ok, Pid } = nktranscoder_protocol:start(Transcoder, ?MODULE),
    nktranscoder_protocol:send(Pid, Req);

nktranscoder_transcode(_SrvId, _Transcoder, _File) ->
    continue.

transcoder_disconnected(Pid) ->
    ?INFO("transcoding disconnected Pid: ~p", [Pid]).

transcoder_connected(Pid) ->
    ?INFO("transcoding connected with Pid: ~p", [Pid]).

transcoder_error(Pid, Msg) ->
    ?INFO("transcoding error ~p with Pid: ~p", [Msg, Pid]).

transcoder_invalid(Pid, Msg) ->
    ?INFO("transcoding invalid ~p with Pid: ~p", [Msg, Pid]).

transcoder_finished(Pid, Msg) ->
    ?INFO("transcoding finished ~p with Pid: ~p", [Msg, Pid]).

transcoder_progress(Pid, Msg) ->
    ?INFO("transcoding progress ~p with Pid: ~p", [Msg, Pid]).
