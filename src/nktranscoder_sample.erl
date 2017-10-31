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
-module(nktranscoder_sample).
-behaviour(nktranscoder_protocol).
-export([test/0]).
-export([transcoder_connected/1,
        transcoder_disconnected/1,
        transcoder_error/2,
        transcoder_invalid/2,
        transcoder_finished/2,
        transcoder_progress/2
        ]).

test() -> 
    ok = nktranscoder_protocol:register(),
    Config = #{ config => #{ server => <<"transcoder://s1.netc.io/transcode">>,
                             user => <<"netcomposer">>,
                             password => <<"burgerenhavacoyunawint">>}},
    
    {ok, Pid } = nktranscoder:connect(Config, ?MODULE),
    nktranscoder:transcode(Pid, {s3, <<"/data/file-6kCmH0rnT8VRluj9eNHqf3TG3zB">>}, 
                                {s3, <<"/data/erlang-test.mp4">>},
                                <<"video/x-flv">> ).

transcoder_disconnected(Pid) ->
    io:format("transcoder disconnected Pid: ~p~n", [Pid]).

transcoder_connected(Pid) ->
    io:format("transcoder connected with Pid: ~p~n", [Pid]).

transcoder_error(Pid, Msg) ->
    io:format("transcoder error ~p with Pid: ~p~n", [Msg, Pid]).

transcoder_invalid(Pid, Msg) ->
    io:format("transcoder invalid ~p with Pid: ~p~n", [Msg, Pid]).

transcoder_finished(Pid, Msg) ->
    io:format("transcoder finished ~p with Pid: ~p~n", [Msg, Pid]).

transcoder_progress(Pid, Msg) ->
    io:format("transcoder progress ~p with Pid: ~p~n", [Msg, Pid]).
