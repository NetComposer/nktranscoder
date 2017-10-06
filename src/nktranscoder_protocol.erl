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
-module(nktranscoder_protocol).
-export([register/0]).
-export([transports/1, default_port/1, conn_init/1, conn_parse/3, conn_encode/2]).
-export([start/2, send/2]).
-export([behaviour_info/1]).
-include_lib("nkpacket/include/nkpacket.hrl").

behaviour_info(callbacks) ->
    [{transcoder_connected, 1}, 
     {transcoder_disconnected, 1},
     {transcoder_error, 2},
     {transcoder_invalid, 2},
     {transcoder_finished, 2},
     {transcoder_progress, 2}
    ].

transports(_) -> [wss, ws].

default_port(ws) -> 80;
default_port(wss) -> 443.

register() -> 
    nkpacket:register_protocol(transcoder, ?MODULE).

start(#{config := #{ server := Server,
                   user := User,
                   password := Password }}, Callback) ->
    AuthToken = base64:encode_to_string(
                 binary_to_list(<<User/binary, <<":">>/binary, Password/binary>>)),
    Headers = [{"Authorization", "Basic " ++ AuthToken}],
    start(Server, Callback, Headers);

start(#{config := #{ server := Server }}, Callback) ->
    start(Server, Callback, []).

start(Server, Callback, Headers) ->
    ConnOpts = #{
      class => transcoder,
      headers => Headers,
      connect_timeout => 60000,
      idle_timeout => 30000,
      user => #{ callback => Callback },
      debug => true },
    case nkpacket:connect(Server, ConnOpts) of
        {ok, Pid} -> 
            {ok, Pid};
        {error, Error} ->
            {error, Error}
    end.

conn_init(#nkport{pid=Pid}=NkPort) ->
    {ok, transcoder, UserData} = nkpacket:get_user(NkPort),
    #{ callback := Mod }=UserData,
    Mod:transcoder_connected(Pid),
    {ok, UserData}.

conn_parse(close, #nkport{pid=Pid}, #{callback := Mod}=State) ->
    Mod:transcoder_disconnected(Pid),
    {ok, State};

conn_parse({text, Text}, #nkport{pid=Pid}, #{callback := Mod}=State) ->
    case nklib_json:decode(Text) of
        {error, E} ->
            Mod:transcoder_error(Pid, E);
        Data when is_map(Data) ->
            notify(Data, Mod, Pid);
        Other ->
            Mod:transcoder_error(Pid, {unexpected, Other})
    end,
    {ok, State}.

notify(#{ <<"code">> := 400, <<"error">> := Reason}, Mod, Pid) ->
    Mod:transcoder_invalid(Pid, Reason);

notify(#{ <<"code">> := 500, <<"error">> := Reason}, Mod, Pid) ->
    Mod:transcoder_error(Pid, Reason);

notify(#{ <<"code">> := 200, <<"event">> := <<"progress">>}=Msg, Mod, Pid) ->
    Mod:transcoder_progress(Pid, Msg);

notify(#{ <<"code">> := 200, <<"event">> := <<"finished">>}=Msg, Mod, Pid) ->
    Mod:transcoder_finished(Pid, Msg);

notify(Msg, Mod, Pid) ->
    Mod:transcoder_error(Pid, {unexpected, Msg}).

conn_encode(Msg, _NkPort) when is_map(Msg) ->
    case nklib_json:encode(Msg) of
        error ->
            lager:warning("invalid json in ~p: ~p", [?MODULE, Msg]),
            {error, invalid_json};
        Json ->
            {ok, {text, Json}}
    end.

send(Conn, Msg) when is_map(Msg) ->
    nkpacket:send(Conn, Msg); 

send(_, Msg) -> 
    {error, invalid_format, Msg}.

