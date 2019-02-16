port module LocalStorage exposing (get, set, toStorage)

import Json.Decode as Decode
import Json.Encode as Encode


get : String -> Cmd msg
get key =
    toStorage
        (Encode.object
            [ ( "type", Encode.string "getItem" )
            , ( "key", Encode.string key )
            , ( "data", Encode.null )
            ]
        )


set : String -> String -> Cmd msg
set key value =
    toStorage
        (Encode.object
            [ ( "type", Encode.string "setItem" )
            , ( "key", Encode.string key )
            , ( "data", Encode.string value )
            ]
        )



-- Ports


port toStorage : Decode.Value -> Cmd msg
