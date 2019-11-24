module Main exposing (Model, Msg(..), SearchResult(..), init, main, subscriptions, update, view, viewResult)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)



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
    { searchResult : SearchResult, searchText : String }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { searchResult = WaitingForInput, searchText = "" }, Cmd.none )


type Pokemon
    = Pokemon
        { imageUrl : String
        , name : String
        }



-- UPDATE


type Msg
    = Search
    | ChangeSearchText String
    | GotPokemon (Result Http.Error Pokemon)


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
            ( { model | searchResult = Loading }, Http.get { url = getPokemonUrl model.searchText, expect = Http.expectJson GotPokemon pokemonFromJSON } )

        GotPokemon result ->
            case result of
                Ok pokemon ->
                    ( { model | searchResult = Success pokemon }, Cmd.none )

                Err _ ->
                    ( { model | searchResult = Failure }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ viewSearchInput model.searchText, button [ onClick Search ] [ text "Search!" ], viewResult model.searchResult ]


viewSearchInput : String -> Html Msg
viewSearchInput searchText =
    Html.form [ onSubmit Search ]
        [ input [ placeholder "Enter an url", value searchText, onInput ChangeSearchText ] []
        ]


viewResult : SearchResult -> Html Msg
viewResult result =
    case result of
        WaitingForInput ->
            text ""

        Failure ->
            text "There was an error trying to load the data :("

        Loading ->
            text "Loading..."

        Success pokemon ->
            viewPokemon pokemon


viewPokemon : Pokemon -> Html Msg
viewPokemon (Pokemon pokemon) =
    pre [] [ text pokemon.name, img [ src pokemon.imageUrl ] [] ]


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
