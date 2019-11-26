module Main exposing (Model, Msg(..), SearchResult(..), init, main, subscriptions, update, view, viewResult)

import Browser
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, index, int, list, string)
import List exposing (..)
import Task exposing (..)


getGrowthRate : GrowthRateSpeed -> Task Http.Error GrowthRate
getGrowthRate speed =
    Http.task
        { method = "GET"
        , headers = []
        , url = pokeApiUrl ++ "growth-rate/" ++ speedName speed
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse (growthRateFromJSON speed)
        , timeout = Nothing
        }


getAllGrowthRates : Task Http.Error (List GrowthRate)
getAllGrowthRates =
    Task.sequence (List.map getGrowthRate allSpeeds)


growthRateFromJSON : GrowthRateSpeed -> Decoder GrowthRate
growthRateFromJSON speed =
    Json.Decode.map2 (\pokemons expPerLevel -> GrowthRate { speed = speed, pokemons = pokemons, expPerLevel = expPerLevel })
        (field "pokemon_species" (Json.Decode.list (field "name" string)))
        expPerLevelFromGrowthRateJSON


expPerLevelFromGrowthRateJSON : Decoder (Dict Int Int)
expPerLevelFromGrowthRateJSON =
    Json.Decode.map Dict.fromList <|
        field "levels" (Json.Decode.list (Json.Decode.map2 (\exp level -> ( level, exp )) (field "experience" int) (field "level" int)))


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { exp : Int, searchResult : SearchResult, searchText : String, growthRates : List GrowthRate }


growthRatesLoaded : Model -> Bool
growthRatesLoaded model =
    not <| List.isEmpty model.growthRates


growthRateForPokemon : Pokemon -> List GrowthRate -> Maybe GrowthRate
growthRateForPokemon pokemon growthRates =
    head <| List.filter (forPokemon pokemon) growthRates


forPokemon : Pokemon -> GrowthRate -> Bool
forPokemon (Pokemon pokemon) (GrowthRate growthRate) =
    List.member pokemon.name growthRate.pokemons


init : () -> ( Model, Cmd Msg )
init _ =
    ( { exp = 0, searchResult = WaitingForInput, searchText = "", growthRates = [] }, Task.attempt GotAllGrowthRates getAllGrowthRates )


type Pokemon
    = Pokemon
        { imageUrl : String
        , name : String
        }


type GrowthRate
    = GrowthRate { speed : GrowthRateSpeed, pokemons : List String, expPerLevel : Dict Int Int }


type ExpCandy
    = CandyXS
    | CandyS
    | CandyM
    | CandyL
    | CandyXL


allExpCandies =
    [ CandyXS, CandyS, CandyM, CandyL, CandyXL ]


candyName candy =
    case candy of
        CandyXS ->
            "CandyXS"

        CandyS ->
            "CandyS"

        CandyM ->
            "CandyM"

        CandyL ->
            "CandyL"

        CandyXL ->
            "CandyXL"


expGiven : ExpCandy -> Int
expGiven candy =
    case candy of
        CandyXS ->
            100

        CandyS ->
            800

        CandyM ->
            3000

        CandyL ->
            10000

        CandyXL ->
            30000


candiesNeededForExp : ExpCandy -> Int -> Int
candiesNeededForExp candy exp =
    ceiling (toFloat exp / toFloat (expGiven candy))


type GrowthRateSpeed
    = Slow
    | Medium
    | Fast
    | MediumSlow
    | SlowThenVeryFast
    | FastThenVerySlow


allSpeeds : List GrowthRateSpeed
allSpeeds =
    [ Slow, Medium, Fast, MediumSlow, SlowThenVeryFast, FastThenVerySlow ]


speedName : GrowthRateSpeed -> String
speedName speed =
    case speed of
        Slow ->
            "slow"

        Fast ->
            "fast"

        Medium ->
            "medium"

        MediumSlow ->
            "medium-slow"

        SlowThenVeryFast ->
            "slow-then-very-fast"

        FastThenVerySlow ->
            "fast-then-very-slow"


speedFromName : String -> GrowthRateSpeed
speedFromName name =
    case name of
        "slow" ->
            Slow

        "fast" ->
            Fast

        "medium" ->
            Medium

        "medium slow" ->
            MediumSlow

        "slow slowthen-very-fast" ->
            SlowThenVeryFast

        "fast-then-very-slow" ->
            FastThenVerySlow

        _ ->
            Debug.todo "no debería pasar nunca por acá"



-- UPDATE


type Msg
    = Search
    | ChangeSearchText String
    | GotAllGrowthRates (Result Http.Error (List GrowthRate))
    | GotPokemon (Result Http.Error Pokemon)
    | ChangeExp (Maybe Int)


type SearchResult
    = Failure
    | Loading
    | WaitingForInput
    | Success Pokemon


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearchText newSearchText ->
            ( { model | searchText = newSearchText }, Cmd.none )

        Search ->
            if String.isEmpty model.searchText then
                ( model, Cmd.none )

            else
                ( { model | searchResult = Loading, searchText = "" }, Http.get { url = getPokemonUrl model.searchText, expect = Http.expectJson GotPokemon pokemonFromJSON } )

        GotAllGrowthRates (Ok growthRates) ->
            ( { model | growthRates = growthRates }, Cmd.none )

        GotAllGrowthRates (Err x) ->
            ( { model | searchResult = Failure }, Cmd.none )

        GotPokemon (Ok pokemon) ->
            ( { model | searchResult = Success pokemon }, Cmd.none )

        GotPokemon (Err _) ->
            ( { model | searchResult = Failure }, Cmd.none )

        ChangeExp (Just newExp) ->
            ( { model | exp = newExp }, Cmd.none )

        ChangeExp Nothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "text-align" "center" ]
        [ viewSearchInput model
        , viewActualExpInput model.exp
        , viewResult model
        ]


viewActualExpInput : Int -> Html Msg
viewActualExpInput exp =
    div []
        [ text "Exp actual"
        , Html.input [ onInput (ChangeExp << String.toInt), attribute "type" "number", value <| String.fromInt exp ] []
        ]


viewSearchInput : Model -> Html Msg
viewSearchInput model =
    Html.form [ onSubmit Search ]
        [ input [ onBlur Search, disabled <| not <| growthRatesLoaded model, placeholder "Pone un pokemon...", value model.searchText, onInput ChangeSearchText, autofocus True ] []
        ]


viewResult : Model -> Html Msg
viewResult model =
    case model.searchResult of
        WaitingForInput ->
            text ""

        Failure ->
            text "There was an error trying to load the data :("

        Loading ->
            text "Loading..."

        Success pokemon ->
            viewPokemon pokemon model.exp model.growthRates


viewGrowthRate : Int -> GrowthRate -> Html Msg
viewGrowthRate actualExp (GrowthRate growthRate) =
    let
        nivelActual =
            expNeededUntilLevel |> Dict.filter (\_ exp -> exp <= 0) |> Dict.keys |> List.maximum |> Maybe.withDefault 1

        expNeededUntilLevel =
            growthRate.expPerLevel |> Dict.map (\_ exp -> exp - actualExp)
    in
    div []
        [ h2 [] [ text <| "Growth Rate speed: " ++ speedName growthRate.speed ]
        , h2 [] [ text <| "Nivel actual: " ++ String.fromInt nivelActual ]
        , expNeededUntilLevel
            |> Dict.filter (\_ exp -> exp > 0)
            |> viewExpRequeridaPorNivel
        ]


viewPokemon : Pokemon -> Int -> List GrowthRate -> Html Msg
viewPokemon (Pokemon pokemon) exp growthRates =
    div []
        [ h1 [] [ text pokemon.name ]
        , div [] [ img [ src pokemon.imageUrl ] [] ]
        , viewGrowthRateForPokemon (Pokemon pokemon) exp growthRates
        ]


viewGrowthRateForPokemon : Pokemon -> Int -> List GrowthRate -> Html Msg
viewGrowthRateForPokemon pokemon exp growthRates =
    case growthRateForPokemon pokemon growthRates of
        Nothing ->
            div [] []

        Just growthRate ->
            viewGrowthRate exp growthRate


viewExpRequeridaPorNivel : Dict Int Int -> Html Msg
viewExpRequeridaPorNivel expRequeridaPorNivel =
    ul []
        (List.map
            (\( nivel, exp ) ->
                li []
                    [ h3 [] [ text <| "Nivel: " ++ String.fromInt nivel ]
                    , h4 [] [ text <| "Experiencia necesaria: " ++ String.fromInt exp ]
                    , h4 [] [ text <| "Caramelos necesarios para alcanzar el nivel" ]
                    , pre [ style "display" "flex", style "flex-direction" "column" ] (List.map (\candy -> h4 [] [ text <| candyName candy ++ ": " ++ String.fromInt (candiesNeededForExp candy exp) ]) allExpCandies)
                    ]
            )
         <|
            toList <|
                expRequeridaPorNivel
        )


pokemonFromJSON : Decoder Pokemon
pokemonFromJSON =
    Json.Decode.map2 (\imageUrl name -> Pokemon { imageUrl = imageUrl, name = name })
        (field "sprites" (field "front_default" string))
        (field "name" string)


pokeApiUrl : String
pokeApiUrl =
    "https://pokeapi.co/api/v2/"


getPokemonUrl : String -> String
getPokemonUrl pokemonName =
    pokeApiUrl ++ "pokemon/" ++ pokemonName
