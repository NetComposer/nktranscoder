# NkTRANSCODER

A video transcoding plugin for NetComposer

## Supported conversions

* MP4 to MP4
* AVI to MP4
* FLV to MP4
* AAC to MP3

## API

There are two main functions offered by this plugin:

* `parse_transcoder/3` returns a video transcoder configuration
* `transcode/3` performs an actual video transcoding, using a transcoder.

## Sample transcoder configuration

A transcoder describes what kind of underlying implementation or provider will be used to perform actual video conversion. As an example, `nktranscoder_ffmpeg` supports the following syntax:

```
Processor = #{ class => ffmpeg,
               config => #{ host => <<"...">>,
                            port => <<"...">>,
                            path => <<"...">>,
                            scheme => transcoder,
                            user => <<"...">>,
                            password => <<"...">> }}
```

where: 

* `user` and `password` are required only for HTTP basic authentication.
* `scheme` is set to `transcoder` by default. This implementation relies on NkPACKET's websocket client using the `transcoder://` protocol.


## Starting a new video transcoding

A request for a new transcoding looks like this:

```
Req => #{ callback => {M, F, A},
          input => #{ type => <<"s3">>,
                      path => InputFileId,
                      content_type => <<"video/avi">> },
          output => #{ type => <<"s3">>,
                       path => OutputFileId,
                       content_type => <<"video/mp4">> }
        },

{ok, Pid} = nktranscoder:transcode(SrvId, Transcoder, Req).
```

where:

* `InputFileId` is the id of the input file, to be read from Netcomposer's S3 file store.
* `OutputFileId` is the file id for the result from the the transcoding process, to be written to Netcomposer's S3 file store.
* `Pid` is the Erlang process that owns the transcoding process.

## Receiving video transcoding events

In the above example, `callback` is an Erlang module (M), function (F) and arguments (A) tuple where to notify transcodings events to. The specified list of arguments will be merged with the event data (Status, Pid and ExtraInfo), received from the transcoder provider. This is so that the application code can correlate transcoder events to the appropriate job request.

If for example we call the transcoding with: 


```
Req => #{ callback => { mymodule, transcoding_event, [JobId] },
          ... 
        },

{ok, Pid} = nktranscoder:transcode(SrvId, Transcoder, Req).
```

then we can write a callback module in the form:

```
-module(mymodule).
-export([transcoding_event/1]).

transcoding_event([ JobId, Status, Pid, ExtraInfo]) ->
    io:format("got transcoding event ~p with Pid: ~p, JobId: ~p, Msg: ~p~n", 
              [Status, Pid, JobId, ExtraInfo]).
```

where:

* `Status` is one of: `<<"invalid">>`, `<<"progress">>`, `<<"finished">>`, or `<<"error">>`.
* `Pid` is the Erlang process that owns the transcoding process.
* `ExtraInfo` is an Erlang term holding extra info about the transcoding processing. 

## Plugins

* `nktranscoder_ffmpeg`, based on a custom FFMPEG based microservice with a Websocket api. 
