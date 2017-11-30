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
-module(nktranscoder_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("nktranscoder.hrl").

start(_StartType, _StartArgs) ->
    Syntax = #{transcoders => {list, map}},
    case nklib_config:load_env(?APP, Syntax) of
        {ok, _} ->
            {ok, Vsn} = application:get_key(?APP, vsn),
            lager:info("NkTranscoder v~s is starting", [Vsn]),
            ok = nktranscoder_ffmpeg_protocol:register(),
            register_types(),
            {ok, Pid} = nktranscoder_sup:start_link(),
            {ok, Pid};
        {error, Error} ->
            lager:error("Error parsing config: ~p", [Error]),
            error({syntax_error, Error})
    end.

stop(_State) ->
    ok.

register_types() ->
    ok.
