module HandleJSONResponse exposing (..)

import Http exposing (Error(..), Response(..))
import Json.Decode exposing (Decoder)


handleJsonResponse : Decoder a -> Response String -> Result Error a
handleJsonResponse decoder response =
    case response of
        BadUrl_ url ->
            Err (Http.BadUrl url)

        Timeout_ ->
            Err Http.Timeout

        BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        NetworkError_ ->
            Err Http.NetworkError

        GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result
