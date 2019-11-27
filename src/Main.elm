module Main exposing (Model, Msg(..), SearchResult(..), init, main, subscriptions, update, view, viewResult)

import Array exposing (Array)
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
import Pokemon exposing (Pokemon(..), growthRateForPokemon, pokemonFromJSON)
import Task exposing (..)


type alias Level =
    Int


type alias CollapsableLevels =
    Dict Level Collapsed


levelCollapsed : Level -> CollapsableLevels -> Collapsed
levelCollapsed level collapsableLevels =
    collapsableLevels
        |> Dict.get level
        |> Maybe.withDefault Collapsed


toggleCollapsed : Collapsed -> Collapsed
toggleCollapsed collapsed =
    case collapsed of
        Collapsed ->
            Extended

        Extended ->
            Collapsed



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
    { exp : Int, searchResult : SearchResult, searchText : String, growthRates : List GrowthRate, levels : CollapsableLevels }


growthRatesLoaded : Model -> Bool
growthRatesLoaded model =
    not <| List.isEmpty model.growthRates


pokemonActualLevel : GrowthRate -> Model -> Int
pokemonActualLevel growthRate model =
    model
        |> expNeededUntilLevels growthRate
        |> Dict.filter (\_ exp -> exp <= 0)
        |> Dict.keys
        |> List.maximum
        |> Maybe.withDefault 1


expNeededUntilLevels : GrowthRate -> Model -> Dict Level Exp
expNeededUntilLevels (GrowthRate growthRate) model =
    growthRate.expPerLevel |> Dict.map (\_ exp -> exp - model.exp)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { exp = 0, searchResult = WaitingForInput, searchText = "", growthRates = [], levels = Dict.empty }, Task.attempt GotAllGrowthRates getAllGrowthRates )


type Collapsed
    = Collapsed
    | Extended



-- UPDATE


type Msg
    = Search
    | ChangeSearchText String
    | GotAllGrowthRates (Result Http.Error (List GrowthRate))
    | GotPokemon (Result Http.Error Pokemon)
    | ChangeExp (Maybe Int)
    | ToggleLevelCollapsed Level


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
            case growthRateForPokemon pokemon model.growthRates of
                Just (GrowthRate growthRate) ->
                    ( { model | searchResult = Success pokemon, levels = Dict.map (\_ _ -> Collapsed) <| growthRate.expPerLevel }, Cmd.none )

                Nothing ->
                    ( { model | searchResult = Failure }, Cmd.none )

        GotPokemon (Err _) ->
            ( { model | searchResult = Failure }, Cmd.none )

        ChangeExp (Just newExp) ->
            ( { model | exp = newExp }, Cmd.none )

        ChangeExp Nothing ->
            ( model, Cmd.none )

        ToggleLevelCollapsed level ->
            ( { model | levels = Dict.update level (Maybe.map toggleCollapsed) model.levels }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "font-size" "20px", style "padding-top" "2em", style "padding-bottom" "5em", style "text-align" "center", style "backgroundColor" "#5FAC79" ]
        [ viewSearchInput model
        , viewActualExpInput model.exp
        , viewResult model
        ]


viewActualExpInput : Int -> Html Msg
viewActualExpInput exp =
    div []
        [ h4 [] [ text "How much exp does it have?: " ]
        , Html.input [ onInput (ChangeExp << String.toInt), style "font-size" "1em", attribute "type" "number", value <| String.fromInt exp ] []
        ]


viewSearchInput : Model -> Html Msg
viewSearchInput model =
    Html.form [ onSubmit Search ]
        [ h4 [] [ text "What's the species of your pokemon?: " ]
        , input [ onBlur Search, disabled <| not <| growthRatesLoaded model, style "font-size" "1em", placeholder "", value model.searchText, onInput ChangeSearchText, autofocus True ] []
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
            viewPokemon pokemon model


viewGrowthRate : Model -> GrowthRate -> Html Msg
viewGrowthRate model growthRate =
    div []
        [ model
            |> expNeededUntilLevels growthRate
            |> Dict.filter (\_ exp -> exp > 0)
            |> viewExpRequeridaPorNivel model.levels
        ]


viewPokemon : Pokemon -> Model -> Html Msg
viewPokemon (Pokemon pokemon) model =
    let
        growthRate =
            model.growthRates
                |> growthRateForPokemonName pokemon.name

        growthRateView =
            growthRate
                |> Maybe.map (viewGrowthRate model)
                |> Maybe.withDefault (div [] [])

        actualLevel =
            Maybe.withDefault 1 <| Maybe.map (\aGrowthRate -> pokemonActualLevel aGrowthRate model) growthRate
    in
    div []
        [ h2 [] [ text <| pokemon.name ++ " Lv. " ++ String.fromInt actualLevel ]
        , div [] [ img [ style "width" "7em", style "height" "7em", src pokemon.imageUrl ] [] ]
        , growthRateView
        ]


possibleColors : Array String
possibleColors =
    Array.fromList
        [ "#6FB9B5", "#C36E6E", "#8FE08C", "#BAC571" ]


colors : List String
colors =
    List.map (\n -> Maybe.withDefault "#FFFFFF" <| Array.get (modBy (Array.length possibleColors) n) possibleColors) (range 1 100)


zip =
    List.map2 Tuple.pair


viewExpRequeridaPorNivel : CollapsableLevels -> Dict Level Exp -> Html Msg
viewExpRequeridaPorNivel collapsableLevels expRequeridaPorNivel =
    div []
        (expRequeridaPorNivel
            |> toList
            |> List.sort
            |> List.reverse
            |> zip colors
            |> List.map
                (\( color, ( level, exp ) ) ->
                    div [ style "border" "outset", style "background" color ]
                        [ case levelCollapsed level collapsableLevels of
                            Collapsed ->
                                viewCollapsedLevel level

                            Extended ->
                                viewExtendedLevel ( level, exp )
                        ]
                )
        )


viewCollapsedLevel : Level -> Html Msg
viewCollapsedLevel level =
    div [ onClick <| ToggleLevelCollapsed level ] [ h3 [] [ text <| "Level: " ++ String.fromInt level ] ]


viewExtendedLevel ( level, exp ) =
    div []
        [ h3 [ onClick <| ToggleLevelCollapsed level ] [ text <| "Level: " ++ String.fromInt level ]
        , h4 [] [ text <| "Needed exp: " ++ String.fromInt exp ]
        , h4 [] [ text <| "Candies needed to reach the level" ]
        , pre [ style "display" "flex", style "flex-direction" "row", style "justify-content" "center" ]
            (List.map (viewCandyNeededForExp exp) allExpCandies)
        ]
