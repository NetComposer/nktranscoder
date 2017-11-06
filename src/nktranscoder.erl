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

transcode(SrvId, #{obj_id := FileId, <<"file">> := #{ content_type := Mime}=File}) ->
    case nkdomain_file_obj:get_store(File) of
        {ok, _StoreId, #{class := Store}} ->
            case SrvId:config() of
                #{ transcoder := Transcoder } ->
                    Args = #{ input => #{ type => Store,
                                          path => FileId, 
                                          content_type => Mime },
                              output => #{ type => Store,
                                           path => nkdomain_file_obj:make_file_id() }},
                    SrvId:nktranscoder_transcode(SrvId, Transcoder, Args);
                _ ->
                    {error, missing_transcoder_config}
            end;
        {error, Error} -> 
            {error, Error}
    end.
