port module Ports
    exposing
        ( reportError
        , writeModule
        )

import Json.Decode exposing (Value)


port reportError : Value -> Cmd msg


port writeModule : Value -> Cmd msg
