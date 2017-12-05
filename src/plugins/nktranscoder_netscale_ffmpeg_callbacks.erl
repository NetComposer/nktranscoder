%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 NetScale, SL.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(nktranscoder_netscale_ffmpeg_callbacks).
-export([nktranscoder_transcode/3, 
         nktranscoder_parse_transcoder/2]).
-include("../../include/nktranscoder.hrl").

nktranscoder_parse_transcoder(Config, Opts) ->
    nktranscoder_netscale_ffmpeg:parse_transcoder(Config, Opts).

nktranscoder_transcode(_SrvId, #{ class := ffmpeg }=Transcoder, #{callback := CB}=Args) ->
    Args2 = maps:remove(callback, Args),
    case nktranscoder_netscale_ffmpeg_protocol:start(Transcoder, CB) of
        {ok, Pid} ->
            nktranscoder_netscale_ffmpeg_protocol:send(Pid, Args2);
        {error, Error } ->
            {error, Error}
    end;

nktranscoder_transcode(_SrvId, _Transcoder, _Args) ->
    continue.
