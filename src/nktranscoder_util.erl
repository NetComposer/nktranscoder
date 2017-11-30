-module(nktranscoder_util).
-export([transcoder_syntax/0]).

transcoder_syntax() ->
    #{ class => atom,
       config => map,
       '__mandatory' => [class, config]
     }.
