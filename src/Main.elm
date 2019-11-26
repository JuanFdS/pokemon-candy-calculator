module Main exposing (Model, Msg(..), SearchResult(..), init, main, subscriptions, update, view, viewResult)

import Browser
import Debug exposing (..)
import Dict exposing (..)
import ExpCandy exposing (..)
import GrowthRate exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, at, field, index, int, list, string)
import List exposing (..)
import PokeApi exposing (getPokemonUrl)
import Pokemon exposing (Pokemon(..), pokemonFromJSON)
import Task exposing (..)



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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { exp = 0, searchResult = WaitingForInput, searchText = "", growthRates = [] }, Task.attempt GotAllGrowthRates getAllGrowthRates )



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


getPokemonCommand : String -> List GrowthRate -> Cmd Msg
getPokemonCommand pokemonName growthRates =
    Http.get { url = getPokemonUrl pokemonName, expect = Http.expectJson GotPokemon (pokemonFromJSON growthRates) }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSearchText newSearchText ->
            ( { model | searchText = newSearchText }, Cmd.none )

        Search ->
            if String.isEmpty model.searchText then
                ( model, Cmd.none )

            else
                ( { model | searchResult = Loading, searchText = "" }, getPokemonCommand model.searchText model.growthRates )

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
    let
        growthRateView =
            growthRates
                |> growthRateForPokemonName pokemon.name
                |> Maybe.map (viewGrowthRate exp)
                |> Maybe.withDefault (div [] [])
    in
    div []
        [ h1 [] [ text pokemon.name ]
        , div [] [ img [ src pokemon.imageUrl ] [] ]
        , growthRateView
        ]


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
