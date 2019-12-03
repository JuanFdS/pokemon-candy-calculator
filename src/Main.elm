module Main exposing (Model, Msg(..), init, main, update, view)

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
import RemoteData exposing (..)
import Task exposing (..)


type alias Label =
    String


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
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { exp : Int
    , searchResult : RemoteData String Pokemon
    , searchText : String
    , growthRates : RemoteData String (List GrowthRate)
    , customLevel : Int
    }


actualExpNeededUntilLevels : Model -> Dict Level Exp
actualExpNeededUntilLevels model =
    Maybe.map (expNeededUntilLevels model.exp) (actualGrowthRate model) |> Maybe.withDefault Dict.empty


actualGrowthRate : Model -> Maybe GrowthRate
actualGrowthRate model =
    RemoteData.map2 growthRateForPokemon model.searchResult model.growthRates
        |> RemoteData.withDefault Nothing


actualLevel : Model -> Level
actualLevel model =
    Maybe.map (levelFromExpAndGrowthRate model.exp) (actualGrowthRate model) |> Maybe.withDefault 1


levelFromExpAndGrowthRate : Exp -> GrowthRate -> Int
levelFromExpAndGrowthRate actualExp growthRate =
    growthRate
        |> expNeededUntilLevels actualExp
        |> Dict.filter (\_ exp -> exp <= 0)
        |> Dict.keys
        |> List.maximum
        |> Maybe.withDefault 1


expNeededUntilLevels : Exp -> GrowthRate -> Dict Level Exp
expNeededUntilLevels actualExp (GrowthRate growthRate) =
    growthRate.expPerLevel |> Dict.map (\_ exp -> exp - actualExp)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { exp = 0, searchResult = NotAsked, searchText = "", growthRates = Loading, customLevel = 50 }, Task.attempt GotAllGrowthRates getAllGrowthRates )


type Collapsed
    = Collapsed
    | Extended



-- UPDATE


type Msg
    = Search
    | ChangeSearchText String
    | GotAllGrowthRates (Result Http.Error (List GrowthRate))
    | GotPokemon (Result Http.Error Pokemon)
    | ChangeExp Int
    | ChangeCustomLevel Int
    | DoNothing


getPokemonCommand : String -> RemoteData String (List GrowthRate) -> Cmd Msg
getPokemonCommand pokemonName requestedGrowthRates =
    case requestedGrowthRates of
        Success growthRates ->
            Http.get { url = getPokemonUrl pokemonName, expect = Http.expectJson GotPokemon (pokemonFromJSON growthRates) }

        _ ->
            Cmd.none


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
            ( { model | growthRates = Success growthRates }, getPokemonCommand "pikachu" model.growthRates )

        GotAllGrowthRates (Err x) ->
            ( { model | growthRates = Failure "Could not fetch growth rates" }, Cmd.none )

        GotPokemon (Ok pokemon) ->
            case RemoteData.toMaybe model.growthRates |> Maybe.andThen (growthRateForPokemon pokemon) of
                Just (GrowthRate growthRate) ->
                    ( { model | searchResult = Success pokemon }, Cmd.none )

                Nothing ->
                    ( { model | searchResult = Failure "No growth rate for pokemon" }, Cmd.none )

        GotPokemon (Err _) ->
            ( { model | searchResult = Failure "Couldn't find pokemon :(" }, Cmd.none )

        ChangeExp newExp ->
            ( { model | exp = newExp }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )

        ChangeCustomLevel newCustomLevel ->
            ( { model | customLevel = newCustomLevel }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style "font-size" "20px", style "display" "flex", style "height" "100vh", style "padding-top" "2em", style "padding-bottom" "5em", style "padding-left" "5em", style "backgroundColor" "lightblue" ]
        [ div [ style "display" "block-inline", style "width" "25em" ]
            [ viewPokemon model
            , viewSearchInput model
            , viewActualExpInput model.exp
            ]
        , viewExpTable model
        ]


viewExpTable model =
    viewWhenPokemon
        { poke = model.searchResult
        , pokemonView = \pokemon -> pokemonTable pokemon model.customLevel (actualExpNeededUntilLevels model)
        }


viewPokemon model =
    div [ style "width" "20em", style "display" "block", style "height" "25em" ]
        [ viewWhenPokemonOr
            { poke = model.searchResult
            , pokemonView = pokemonPicture
            , errorView = \_ -> missignoPicture
            }
        , viewWhenPokemonOr
            { poke = model.searchResult
            , pokemonView = viewPokemonNameAndLevel <| actualLevel model
            , errorView = \error -> h4 [] [ text error ]
            }
        ]


missignoPicture =
    pokemonPicture (Pokemon { name = "MissigNo", imageUrl = "../assets/missignNo.png" })


viewWhenPokemonOr : { poke : RemoteData String Pokemon, pokemonView : Pokemon -> Html Msg, errorView : String -> Html Msg } -> Html Msg
viewWhenPokemonOr { poke, pokemonView, errorView } =
    case RemoteData.map pokemonView poke of
        Success successfulView ->
            successfulView

        Loading ->
            text "Loading..."

        NotAsked ->
            div [] []

        Failure errorMessage ->
            errorView errorMessage


viewWhenPokemon : { poke : RemoteData String Pokemon, pokemonView : Pokemon -> Html Msg } -> Html Msg
viewWhenPokemon { poke, pokemonView } =
    viewWhenPokemonOr { poke = poke, pokemonView = pokemonView, errorView = \_ -> div [] [] }


viewIfSuccess : RemoteData error (Html Msg) -> Html Msg
viewIfSuccess remoteDataView =
    case remoteDataView of
        Success successView ->
            successView

        _ ->
            div [] []


pokemonPicture : Pokemon -> Html Msg
pokemonPicture (Pokemon pokemon) =
    div [ style "position" "relative" ]
        [ img
            ([ style "width" "100%"
             , style "height" "100%"
             , style "background" "white"
             , src pokemon.imageUrl
             ]
                ++ dialogBoxStyle
            )
            []
        , pokeballPicture [ style "bottom" "-5px", style "left" "-10px" ]
        , pokeballPicture [ style "bottom" "-5px", style "right" "-17px" ]
        , pokeballPicture [ style "top" "-9px", style "right" "-17px" ]
        , pokeballPicture [ style "top" "-9px", style "left" "-10px" ]
        ]


dialogBoxStyle =
    [ style "box-shadow" "0 3px 0 3px black, inset 0 3px 0 3px black, 0 0 0 3px black, inset 0 0 0 3px black"
    , style "border" "3px solid white"
    , style "border-radius" "4px"
    , style "background" "white"
    ]


viewPokemonNameAndLevel : Level -> Pokemon -> Html Msg
viewPokemonNameAndLevel level (Pokemon pokemon) =
    h4 [ style "text-align" "center" ] [ text <| "Lv. " ++ toString level ++ " " ++ pokemon.name ]


pokeballPicture : List (Html.Attribute Msg) -> Html Msg
pokeballPicture attributes =
    img ([ src "../assets/pokeball.png", style "image-rendering" "crisp-edges", style "position" "absolute", style "height" "25px" ] ++ attributes) []


pokemonTable : Pokemon -> Level -> Dict Level Exp -> Html Msg
pokemonTable (Pokemon pokemon) customLevel expUntilLevelsAreReached =
    let
        nextLevel =
            expUntilLevelsAreReached
                |> Dict.filter (\level exp -> exp > 0)
                |> Dict.toList
                |> List.head
                |> Maybe.withDefault ( 100, 0 )
                |> (\( level, exp ) -> ( "Next level", level, exp ))

        level100 =
            expUntilLevelsAreReached
                |> Dict.get 100
                |> Maybe.withDefault 0
                |> (\exp -> ( "Max level", 100, exp ))

        selectedLevel =
            expUntilLevelsAreReached
                |> Dict.get customLevel
                |> Maybe.withDefault 0
                |> (\exp -> ( "Custom level", customLevel, exp ))

        levelsToShowInHeader =
            [ nextLevel, level100 ]

        levelsToShowInBody =
            [ selectedLevel, nextLevel, level100 ]
    in
    div [ style "position" "relative", style "height" "39em" ]
        [ table ([ style "border-collapse" "collapse" ] ++ dialogBoxStyle)
            (levelsTableHeader selectedLevel levelsToShowInHeader :: List.map (levelsTableRow levelsToShowInBody) allExpCandies)
        ]


levelsTableHeader : ( Label, Level, Exp ) -> List ( Label, Level, Exp ) -> Html Msg
levelsTableHeader selectedLevel levels =
    row
        (cell [ h4 [] [ text "Candies needed" ] ]
            :: customLevelHeader selectedLevel
            :: List.map levelHeaderCell levels
        )


customLevelHeader : ( Label, Level, Exp ) -> Html Msg
customLevelHeader ( label, level, exp ) =
    cell
        [ h4 []
            [ text label
            , input
                [ onNumericInput ChangeCustomLevel
                , value (toString level)
                , style "width" "3em"
                , style "text-align" "center"
                ]
                []
            ]
        ]


levelHeaderCell : ( Label, Level, Exp ) -> Html Msg
levelHeaderCell ( label, level, _ ) =
    cell [ h4 [] [ div [] [ text label ], div [] [ text <| toString level ] ] ]


levelsTableRow : List ( Label, Level, Exp ) -> ExpCandy -> Html Msg
levelsTableRow levels candy =
    row
        (cell [ viewCandy candy ] :: List.map (\( _, _, exp ) -> cell [ viewAmountofCandyNeededForExp exp candy ]) levels)


row =
    tr [ style "border" "1px solid lightGray", style "padding" "1em", style "margin" "0" ]


cell =
    td
        [ style "text-align" "center"
        , style "border" "1px solid lightGray"
        , style "width" "5em"
        , style "padding" "1em"
        , style "margin" "0"
        ]


onNumericInput eventConstructor =
    onInput
        (\stringValue ->
            if String.isEmpty stringValue then
                eventConstructor 0

            else
                case String.toInt stringValue of
                    Just value ->
                        eventConstructor value

                    Nothing ->
                        DoNothing
        )


viewActualExpInput : Int -> Html Msg
viewActualExpInput exp =
    div []
        [ h4 [] [ text "How much exp does it have?: " ]
        , Html.input [ onNumericInput ChangeExp, style "font-size" "1em", attribute "type" "number", value <| String.fromInt exp ] []
        ]


viewSearchInput : Model -> Html Msg
viewSearchInput model =
    case model.growthRates of
        Success growthRates ->
            Html.form [ onSubmit Search ]
                [ h4 [] [ text "What's the species of your pokemon?: " ]
                , input [ onBlur Search, style "font-size" "1em", placeholder "", value model.searchText, onInput ChangeSearchText, autofocus True ] []
                ]

        Loading ->
            text "Loading..."

        Failure _ ->
            text "Something went wrong :("

        NotAsked ->
            text "Loading..."
