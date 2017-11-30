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
-module(nktranscoder_ffmpeg).
-export([parse_transcoder/2]).

parse_transcoder(Data, Opts) ->
    case nklib_syntax:parse(Data, #{class=>atom}) of
        {ok, #{class := ffmpeg}, _} ->
            case nklib_syntax:parse(Data, transcoder_syntax(), Opts) of
                {ok, Transcoder, UnknownFields} ->
                    {ok, Transcoder, UnknownFields};
                {error, Error} ->
                    {error, Error}
            end;
        _ ->
            continue
    end.

transcoder_syntax() ->
    Base = nktranscoder_util:transcoder_syntax(),
    Base#{
      config := #{
        host => binary,
        port => integer,
        scheme => atom,
        path => binary,
        user => binary,
        password => binary,
        '__mandatory' => [host, port, scheme, path]
       }
     }.
