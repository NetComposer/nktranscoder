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
-module(nktranscoder).
-export([parse_transcoder/3,
         transcode/3]).
-include("nktranscoder.hrl").

-type transcoder() :: #{ class => atom,
                         config => map() }.
-type content_type() :: binary.
-type store_type() :: binary.
-type file_info() :: #{ type => store_type(),
                      path => binary,
                      content_type => content_type() }.
-type req() :: #{ callback => mfa(),
                  input => file_info() }.


-spec parse_transcoder(nkservice:id(), map(), map()) ->
    {ok, transcoder()} | {error, term()}.

-spec transcode(nkservice:id(), transcoder(), req()) ->
    {ok, pid()} | {error, term()}.

transcode(SrvId, Transcoder, Req) -> 
    SrvId:nktranscoder_transcode(SrvId, Transcoder, Req).

parse_transcoder(SrvId, Config, Opts) ->
    SrvId:nktranscoder_parse_transcoder(Config, Opts).

