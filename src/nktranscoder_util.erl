%% @doc
-module(nktranscoder_util).

-export([read_body/3, write_body/5]).
-export([span_create/2, span_log/1, span_log/2, span_error/1, span_finish/0]).

-include("nktranscoder.hrl").
-include_lib("nkserver/include/nkserver.hrl").
-include_lib("nkserver/include/nkserver_callback.hrl").


-define(LLOG(Type, Txt, Args), lager:Type("NkTRANS "++Txt, Args)).
-define(SPAN_READ, {?MODULE, read_body}).
-define(SPAN_WRITE, {?MODULE, write_body}).

%% ===================================================================
%% Callbacks
%% ===================================================================


%% @private Read body from a nkrest request
read_body(SrvId, #{input_type:=inline}=Params, Req) ->
    BaseSpan = maps:get(ot_span_id, Params, undefined),
    nkserver_ot:new(?SPAN_READ, SrvId, <<"NkTranscoder:ReadInlineBody">>, BaseSpan),
    MaxSize = netcomp_trans_app:get(max_inline_size),
    nkserver_ot:log({?MODULE, ready_body}, <<"reading inline body (max_size:~p)">>, [MaxSize]),
    case nkrest_http:get_body(Req, #{max_size=>MaxSize}) of
        {ok, Body, Req2} ->
            Hds = nkrest_http:get_headers(Req),
            CT = maps:get(<<"content-type">>, Hds, <<>>),
            nkserver_ot:log(?SPAN_READ, <<"body read (ct:~s, size:~p)">>, [CT, byte_size(Body)]),
            nkserver_ot:finish(?SPAN_READ),
            {ok, CT, Body, Req2};
        {error, {body_too_large, _, _}} ->
            nkserver_ot:log(?SPAN_READ, <<"error ready body: too_large">>),
            nkserver_ot:finish(?SPAN_READ),
            {error, body_too_large}
    end;

read_body(SrvId, #{input_type:=http, input_http_url:=Url}=Params, Req) ->
    BaseSpan = maps:get(ot_span_id, Params, undefined),
    nkserver_ot:new(?SPAN_READ, SrvId, <<"NkTranscoder:ReadHTTPBody">>, BaseSpan),
    Opts = [
        {insecure, maps:get(insecure, Params)},
        {pool, default},
        with_body
    ],
    case hackney:request(get, Url, [], <<>>, Opts) of
        {ok, Code, Hds, Body} when Code>=200, Code<300 ->
            CT = find_ct(Hds),
            nkserver_ot:log(?SPAN_READ, <<"body read (ct:~s, size:~p)">>, [CT, byte_size(Body)]),
            nkserver_ot:finish(?SPAN_READ),
            {ok, CT, Body, Req};
        {ok, Code, _, _} ->
            nkserver_ot:log(?SPAN_READ, <<"invalid return code: ~p">>, [Code]),
            lager:notice("Error calling url '~s': ~p", [Url, Code]),
            nkserver_ot:finish(?SPAN_READ),
            {error, http_error};
        {error, Error} ->
            nkserver_ot:log(?SPAN_READ, <<"invalid return: ~p">>, [Error]),
            lager:notice("Error calling url '~s': ~p", [Url, Error]),
            nkserver_ot:finish(?SPAN_READ),
            {error, http_error}
    end;

read_body(SrvId, #{input_type:=s3}=Params, Req) ->
    BaseSpan = maps:get(ot_span_id, Params, undefined),
    nkserver_ot:new(?SPAN_READ, SrvId, <<"NkTranscoder:ReadS3Body">>, BaseSpan),
    #{
        input_s3_bucket := Bucket,
        input_s3_key := Key,
        input_s3_secret := Secret,
        input_s3_path := Path
    } = Params,
    S3Config1 = #{
        key => Key,
        secret => Secret
    },
    S3Config2 = case Params of
        #{input_s3_url:=S3Url} ->
            S3Config1#{url=>S3Url};
        _ ->
            S3Config1
    end,
    S3Config3 = case Params of
        #{input_s3_region:=Region} ->
            S3Config2#{region=>Region};
        _ ->
            S3Config2
    end,
    nkserver_ot:log(?SPAN_READ, <<"reading S3 body (~s:~s)">>, [Bucket, Path]),
    {Method, Url, Headers} = nkaws_s3:get_object(Bucket, Path, S3Config3),
    Opts = [
        {insecure, maps:get(insecure, Params)},
        {pool, default},
        with_body
    ],
    case hackney:request(Method, Url, Headers, <<>>, Opts) of
        {ok, Code, Hds, Body} when Code>=200, Code<300 ->
            CT = find_ct(Hds),
            nkserver_ot:log(?SPAN_READ, <<"body read (ct:~s, size:~p)">>, [CT, byte_size(Body)]),
            nkserver_ot:finish(?SPAN_READ),
            {ok, CT, Body, Req};
        {ok, Code, _, _} ->
            nkserver_ot:log(?SPAN_READ, <<"invalid return code: ~p">>, [Code]),
            lager:notice("Error calling url '~s': ~p", [Url, Code]),
            nkserver_ot:finish(?SPAN_READ),
            {error, http_error};
        {error, Error} ->
            nkserver_ot:log(?SPAN_READ, <<"invalid return: ~p">>, [Error]),
            lager:notice("Error calling url '~s': ~p", [Url, Error]),
            nkserver_ot:finish(?SPAN_READ),
            {error, http_error}
    end.


%% @private
write_body(_SrvId, #{output_type:=inline}, CT, Body, Req) ->
    {ok, CT, Body, Req};

write_body(SrvId, #{output_type:=http, output_http_url:=Url}=Params, CT, Body, Req) ->
    BaseSpan = maps:get(ot_span_id, Params, undefined),
    nkserver_ot:new(?SPAN_READ, SrvId, <<"NkTranscoder:WriteHTTPBody">>, BaseSpan),
    nkserver_ot:log(?SPAN_WRITE, <<"writing external body (~s)">>, [Url]),
    Opts = [
        {insecure, maps:get(insecure, Params)},
        {pool, default},
        with_body
    ],
    Hds = [{<<"content-type">>, CT}],
    case hackney:request(post, Url, Hds, Body, Opts) of
        {ok, Code, _RepHds, _RepBody} when Code>=200, Code<300 ->
            nkserver_ot:log(?SPAN_WRITE, <<"body written (ct:~s, size:~p)">>, [CT, byte_size(Body)]),
            nkserver_ot:finish(?SPAN_READ),
            {ok, <<>>, <<>>, Req};
        {ok, Code, RepHds, RepBody} ->
            nkserver_ot:log(?SPAN_WRITE, <<"invalid return code: ~p">>, [Code]),
            lager:notice("Error calling url '~s': ~p", [Url, Code]),
            lager:notice("HDS: ~p\nBody: ~s", [RepHds, RepBody]),
            nkserver_ot:finish(?SPAN_READ),
            {error, http_error};
        {error, Error} ->
            nkserver_ot:log(?SPAN_WRITE, <<"invalid return: ~p">>, [Error]),
            lager:notice("Error calling url '~s': ~p", [Url, Error]),
            nkserver_ot:finish(?SPAN_READ),
            {error, http_error}
    end;

write_body(SrvId, #{output_type:=s3}=Params, CT, Body, Req) ->
    BaseSpan = maps:get(ot_span_id, Params, undefined),
    nkserver_ot:new(?SPAN_READ, SrvId, <<"NkTranscoder:WriteS3Body">>, BaseSpan),
    #{
        output_s3_bucket := Bucket,
        output_s3_key := Key,
        output_s3_secret := Secret,
        output_s3_path := Path
    } = Params,
    S3Config1 = #{
        key => Key,
        secret => Secret
    },
    S3Config2 = case Params of
        #{output_s3_url:=S3Url} ->
            S3Config1#{url=>S3Url};
        _ ->
            S3Config1
    end,
    S3Config3 = case Params of
        #{output_s3_region:=Region} ->
            S3Config2#{region=>Region};
        _ ->
            S3Config2
    end,
    nkserver_ot:log(?SPAN_WRITE, <<"writting S3 body (~s:~s)">>, [Bucket, Path]),
    Hash = crypto:hash(sha256, Body),
    {Method, Url, Headers} = nkaws_s3:put_object(Bucket, Path, CT, Hash, S3Config3),
    Opts = [
        {insecure, maps:get(insecure, Params)},
        {pool, default},
        with_body
    ],
    case hackney:request(Method, Url, Headers, Body, Opts) of
        {ok, Code, _RepHds, RepBody} when Code>=200, Code<300 ->
            nkserver_ot:log(?SPAN_WRITE, <<"body written (size:~p)">>, [byte_size(RepBody)]),
            nkserver_ot:finish(?SPAN_READ),
            {ok, <<>>, Body, Req};
        {ok, Code, RepHds, RepBody} ->
            nkserver_ot:log(?SPAN_WRITE, <<"invalid return code: ~p">>, [Code]),
            lager:notice("Error calling url '~s': ~p", [Url, Code]),
            lager:notice("HDS: ~p\nBody: ~s", [RepHds, RepBody]),
            nkserver_ot:finish(?SPAN_READ),
            {error, http_error};
        {error, Error} ->
            nkserver_ot:log(?SPAN_WRITE, <<"invalid return: ~p">>, [Error]),
            lager:notice("Error calling url '~s': ~p", [Url, Error]),
            nkserver_ot:finish(?SPAN_READ),
            {error, http_error}
    end.



%% @private
find_ct([]) ->
    <<>>;

find_ct([{<<First, _/binary>>=Key, Value}|Rest]) when First==$c;First==$C ->
    case nklib_util:to_lower(Key) of
        <<"content-type">> ->
            Value;
        _ ->
            find_ct(Rest)
    end;

find_ct([{_, _}|Rest]) ->
    find_ct(Rest).


%% @private
span_create(SrvId, BaseSpan) ->
    nkserver_ot:new(?REQ_SPAN, SrvId, <<"NkTranscoder::Transcode">>, BaseSpan).


%% @private
span_finish() ->
    nkserver_ot:finish(?REQ_SPAN).


%% @private
span_log(Log) ->
    nkserver_ot:log(?REQ_SPAN, Log).


%% @private
span_log(Txt, Data) ->
    nkserver_ot:log(?REQ_SPAN, Txt, Data).


%%%% @private
%%span_tags(Tags) ->
%%    nkserver_ot:tags(?REQ_SPAN, Tags).


%% @private
span_error(Error) ->
    nkserver_ot:tag_error(?REQ_SPAN, Error).


