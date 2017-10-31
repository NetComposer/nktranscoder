%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Carlos Gonzalez Florido.  All Rights Reserved.
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
-module(nktranscoder).
-export([transcode/2]).

-spec transcode(nkservice:id(), nkfile:file()) ->
    ok | {error, term()}.

transcode(SrvId, File) ->
    case nkfile:download(SrvId, File) of
        {ok, File, _Bin} ->
            case SrvId:config() of
                #{ transcoder := Transcoder } ->
                    SrvId:nktranscoder_transcode(SrvId, Transcoder, File);
                _ ->
                    {error, missing_transcoder_config}
            end;
        {error, Error} ->
            {error, Error}
    end.
