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

Notes: 

* `user` and `password` are required only for HTTP basic authentication.
* `scheme` is set to `transcoder` by default. This implementation relies on NkPACKET's websocket client using the `transcoder://` protocol.


## Sample video transcoding request

Starting a new transcoding is as easy as doing:

```
Req => #{ callback => { M, F, Args },
          input => #{ type => <<"s3">>,
                      path => InputFileId,
                      content_type => <<"video/avi">> },
          output => #{ type => <<"s3">>,
                       path => OutputFileId,
                       content_type => <<"video/mp4">> }
        },

{ok, Pid} = nktranscoder:transcode(SrvId, Transcoder, Req).
```

where `InputFileId` is the id of the input file, and `OutputFileId` is the file id for the result from the the transcoding process. The above request indicates both files should be read from and written to Netcomposer's S3 file store.


## Receiving video transcoding events





## Plugins

* `nktranscoder_ffmpeg`, based on a remote FFMPEG based microservice with a Websocket api. 
