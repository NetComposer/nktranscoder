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
