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

-module(nktranscoder_callbacks).
-export([msg/1, transcoder_operation/5]).
-include("nktranscoder.hrl").


%% ===================================================================
%% Msg Callbacks
%% ===================================================================

msg(_)   		                    -> continue.


transcoder_operation(SrvId, <<"echo">>, CT, File, Params) ->
    BaseSpan = maps:get(ot_span_id, Params, undefined),
    nkserver_ot:new(?MODULE, SrvId, <<"NkTranscoder::Echo">>, BaseSpan),
    nkserver_ot:log(?MODULE, "performing 'echo'"),
    nkserver_ot:finish(?MODULE),
    {ok, {CT, File, #{}}};

transcoder_operation(_SrvId, _Op, _CT, _File, _Params) ->
    {error, operation_invalid}.


