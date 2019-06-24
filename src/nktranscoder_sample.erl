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

-module(nktranscoder_sample).
-compile([export_all, nowarn_export_all]).

-define(ECHO_URL, "http://127.0.0.1:9001/transcoder/v1/").

-define(AWS_KEY, "AKIA*").
-define(AWS_SECRET, "19Ii*").
-define(AWS_BUCKET, "nkobjects").




%%% TESTS
all_tests() ->
    test_inline_inline(),
    test_inline_http(),
    test_http_inline(),
    test_inline_s3(),
    test_s3_inline(),
    ok.




%% @doc
% File in http body, response in http response
test_inline_inline() ->
    Hds = [{<<"Content-Type">>, <<"ct1/ct2">>}],
    Body = <<"1234">>,
    {T1, {ok, 200, RepHds, Body}} = call_echo(#{}, Hds, Body),
    <<"ct1/ct2">> = nklib_util:get_value(<<"content-type">>, RepHds),
    {T1, RepHds}.


%% @doc
% File in http body, response sent to external HTTP
test_inline_http() ->
    Hds = [{<<"Content-Type">>, <<"ct4/ct5">>}],
    Body = <<"_test_upload">>,
    Params = #{
        output_type => http,
        output_http_url => <<"http://127.0.0.1:9001/_test_upload">>
    },
    {T1, {ok, 200, _, _}} = call_echo(Params, Hds, Body),
    T1.


%% @doc
% File in external http, response in http response
test_http_inline() ->
    Params = #{
        format => <<"var=1;var2=a">>,
        input_type => http,
        input_http_url => <<"http://127.0.0.1:9001/_test_download">>
    },
    {T1, {ok, 200, RepHds, <<"test_download">>}} = call_echo(Params, [], <<>>),
    <<"test/download">> = nklib_util:get_value(<<"content-type">>, RepHds),
    {T1, RepHds}.



% For S3 tests, start minio:
% export MINIO_ACCESS_KEY=5UBED0Q9FB7MFZ5EWIOJ; export MINIO_SECRET_KEY=CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI; minio server .
% mc config host add myminio http://192.168.1.42:9000 5UBED0Q9FB7MFZ5EWIOJ CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI; mc mb bucket1

%% @doc
% File in http body, response sent to S3
test_inline_s3() ->
    Hds = [{<<"Content-Type">>, <<"ct5/ct6">>}],
    Body = <<"_test_file">>,
    Params = #{
        output_type => s3,
        output_s3_bucket => bucket1,
        output_s3_url => "http://127.0.0.1:9000",
        output_s3_key => "5UBED0Q9FB7MFZ5EWIOJ",
        output_s3_secret => "CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI",
        output_s3_path => "/test"
    },
    {T1, {ok, 200, _, _}} = call_echo(Params, Hds, Body),
    T1.


%% @doc
% File in S3, response in http response
test_s3_inline() ->
    Params = #{
        input_type => s3,
        input_s3_bucket => bucket1,
        input_s3_url => "http://127.0.0.1:9000",
        input_s3_key => "5UBED0Q9FB7MFZ5EWIOJ",
        input_s3_secret => "CaK4frX0uixBOh16puEsWEvdjQ3X3RTDvkvE+tUI",
        input_s3_path => "/test"
    },
    {T1, {ok, 200, RepHds, <<"_test_file">>}} = call_echo(Params, [], <<>>),
    <<"ct5/ct6">> = nklib_util:get_value(<<"content-type">>, RepHds),
    {T1, RepHds}.



%% @doc
% File in http body, response sent to S3
test_inline_aws() ->
    Hds = [{<<"Content-Type">>, <<"ct6/ct7">>}],
    Body = <<"_test_file">>,
    Params = #{
        output_type => s3,
        output_s3_bucket => ?AWS_BUCKET,
        output_s3_key => ?AWS_KEY,
        output_s3_secret => ?AWS_SECRET,
        output_s3_path => "/test",
        output_s3_region => "eu-central-1"
    },
    {T1, {ok, 200, _, _}} = call_echo(Params, Hds, Body),
    T1.


%% @doc
% File in S3, response in http response
test_aws_inline() ->
    Params = #{
        input_type => s3,
        input_s3_bucket => ?AWS_BUCKET,
        input_s3_key => ?AWS_KEY,
        input_s3_secret => ?AWS_SECRET,
        input_s3_path => "/test",
        input_s3_region => "eu-central-1"
    },
    {T1, {ok, 200, RepHds, <<"_test_file">>}} = call_echo(Params, [], <<>>),
    <<"ct6/ct7">> = nklib_util:get_value(<<"content-type">>, RepHds),
    {T1, RepHds}.



%%% Internal


%% @private
call_echo(Params, Hds, Body) ->
    call(<<"echo">>, Params, Hds, Body).


%% @private
call(Cmd, Params, Hds, Body) ->
    Params2 = [
        list_to_binary([
            nklib_util:to_binary(K),
            $=,
            http_uri:encode(nklib_util:to_binary(V))
        ])
        || {K, V} <- maps:to_list(Params)
    ],
    Params3 = nklib_util:bjoin(Params2, <<"&">>),
    Url = list_to_binary(case Params2 of
        [] ->
            [?ECHO_URL, Cmd];
        _ ->
            [?ECHO_URL, Cmd, "?", Params3]
    end),
    lager:notice("SAMPLE Calling URL ~s", [Url]),
    Opts = [with_body],
    Start = nklib_date:epoch(usecs),
    R = hackney:request(post, Url, Hds, Body, Opts),
    Time = nklib_date:epoch(usecs) - Start,
    {Time, R}.
