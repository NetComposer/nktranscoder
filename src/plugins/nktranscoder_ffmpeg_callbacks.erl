-module(nktranscoder_ffmpeg_callbacks).
-export([nktranscoder_transcode/3, 
         nktranscoder_parse_transcoder/2,
         nktranscoder_event/1]).
-include("../../include/nktranscoder.hrl").

nktranscoder_parse_transcoder(Config, Opts) ->
    nktranscoder_ffmpeg:parse_transcoder(Config, Opts).

nktranscoder_callback() -> ?MODULE.

nktranscoder_transcode(_SrvId, #{ class := ffmpeg }=Transcoder, #{job_id := JobId}=Args) ->
    CB = { nktranscoder_callback(), nktranscoder_event, [JobId]},
    case nktranscoder_ffmpeg_protocol:start(Transcoder, CB) of
        {ok, Pid} ->
            nktranscoder_ffmpeg_protocol:send(Pid, Args);
        {error, Error } ->
            {error, Error}
    end;

nktranscoder_transcode(_SrvId, _Transcoder, _Args) ->
    continue.

nktranscoder_event([JobId, Ev, Pid, Msg]) ->
    ?DEBUG("=> got event ~p with Pid: ~p, JobId: ~p, Msg: ~p", [Ev, Pid, JobId, Msg]),
    case nkdomain_transcoder_job_obj:update(JobId, #{ status => Ev,
                                          pid => Pid,
                                          info => Msg }) of 
        ok -> 
            ?DEBUG("succesfully updated transcoding job ~p", [JobId]);
        {error, Error} ->
            ?ERROR("error while updating transcoding job ~p: ~p", [JobId, Error])
    end.
