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
-module(nktranscoder_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

%% @private
start_link() ->
    ChildsSpec = [
    ],
    supervisor:start_link({local, ?MODULE}, ?MODULE,
        {{one_for_one, 10, 60}, ChildsSpec}).

%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.


