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
-export([parse/1, process/5]).

-include("nktranscoder.hrl").
-include_lib("nkserver/include/nkserver.hrl").
-include_lib("nkserver/include/nkserver_callback.hrl").



%% ===================================================================
%% Public
%% ==================================================================


parse(Params) ->
    ParamSyntax = #{
        input_type => {atom, [inline, s3, http]},
        output_type => {atom, [inline, s3, http]},
        format => tokens,
        callback => binary,
        input_http_url => binary,
        output_http_url => binary,
        input_s3_bucket => {binary, 1, 256},
        input_s3_key => {binary, 1, 256},
        input_s3_secret => {binary, 1, 256},
        input_s3_path => {binary, 1, 1024},
        input_s3_url => {binary, 1, 1024},
        input_s3_region => binary,
        output_s3_bucket => {binary, 1, 256},
        output_s3_key => {binary, 1, 256},
        output_s3_secret => {binary, 1, 256},
        output_s3_path => {binary, 1, 1024},
        output_s3_url => {binary, 1, 1024},
        output_s3_region => binary,
        insecure => boolean,
        '__defaults' => #{
            input_type => inline,
            output_type => inline,
            insecure => false
        },
        '__post_check' => fun post_check_params/1
    },
    case nklib_syntax:parse(Params, ParamSyntax) of
        {ok, QsParsed, _} ->
            {ok, QsParsed};
        {error, Error} ->
            {error, Error}
    end.


%% @doc
process(SrvId, Operation, CT, Body, Params) ->
    BaseSpan = maps:get(ot_span_id, Params, undefined),
    nktranscoder_util:span_create(SrvId, BaseSpan),
    nktranscoder_util:span_log("request parsed: ~p", [Params]),
    Params2 = Params#{ot_span_id => ?REQ_SPAN},
    Args = [SrvId, Operation, CT, Body, Params2],
    case ?CALL_SRV(SrvId, transcoder_operation, Args) of
        {ok, {CT2, File2, Meta}} ->
            nktranscoder_util:span_log("response ready: ~s", [CT]),
            nkserver_ot:finish(?REQ_SPAN),
            {ok, {CT2, File2, Meta}};
        {error, Error} ->
            {error, Error}
    end.




%% ===================================================================
%% Internal
%% ===================================================================

parse_params(Req) ->
    ParamSyntax = #{
        input_type => {atom, [inline, s3, http]},
        output_type => {atom, [inline, s3, http]},
        format => tokens,
        callback => binary,
        input_http_url => binary,
        output_http_url => binary,
        input_s3_bucket => {binary, 1, 256},
        input_s3_key => {binary, 1, 256},
        input_s3_secret => {binary, 1, 256},
        input_s3_path => {binary, 1, 1024},
        input_s3_url => {binary, 1, 1024},
        input_s3_region => binary,
        output_s3_bucket => {binary, 1, 256},
        output_s3_key => {binary, 1, 256},
        output_s3_secret => {binary, 1, 256},
        output_s3_path => {binary, 1, 1024},
        output_s3_url => {binary, 1, 1024},
        output_s3_region => binary,
        insecure => boolean,
        '__defaults' => #{
            input_type => inline,
            output_type => inline,
            insecure => false
        },
        '__post_check' => fun post_check_params/1
    },
    Qs = nkrest_http:get_qs(Req),
    case nklib_syntax:parse(Qs, ParamSyntax) of
        {ok, QsParsed, _} ->
            {ok, QsParsed};
        {error, Error} ->
            {error, Error}
    end.


%% @private
post_check_params(Values) ->
    Map = maps:from_list(Values),
    post_check_params_in(Map).


%% @private
post_check_params_in(#{input_type:=inline}=Map) ->
    post_check_params_out(Map);

post_check_params_in(#{input_type:=http, input_http_url:=_}=Map) ->
    post_check_params_out(Map);

post_check_params_in(#{input_type:=http}) ->
    {error, {field_missing, <<"input_http_url">>}};

post_check_params_in(#{input_type:=s3}=Map) ->
    Syntax = #{
        input_s3_bucket => binary,
        input_s3_key => binary,
        input_s3_secret => binary,
        input_s3_path => binary,
        '__mandatory' => [
            input_s3_bucket,
            input_s3_key,
            input_s3_secret,
            input_s3_path
        ]
    },
    case nklib_syntax:parse(Map, Syntax) of
        {ok, _Parsed, _} ->
            post_check_params_out(Map);
        {error, Error} ->
            {error, Error}
    end.


%% @private
post_check_params_out(#{output_type:=inline}) ->
    ok;

post_check_params_out(#{output_type:=http, output_http_url:=_}) ->
    ok;

post_check_params_out(#{output_type:=http}) ->
    {error, {field_missing, <<"output_url">>}};

post_check_params_out(#{output_type:=s3}=Map) ->
    Syntax = #{
        output_s3_bucket => binary,
        output_s3_key => binary,
        output_s3_secret => binary,
        output_s3_path => binary,
        '__mandatory' => [
            output_s3_bucket,
            output_s3_key,
            output_s3_secret,
            output_s3_path
        ]
    },
    case nklib_syntax:parse(Map, Syntax) of
        {ok, _Parsed, _} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.
