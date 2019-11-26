module GrowthRate exposing (..)

import Dict exposing (Dict(..))
import HandleJSONResponse exposing (handleJsonResponse)
import Http
import Json.Decode exposing (Decoder, field, int, string)
import PokeApi exposing (getGrowthRateUrl)
import Task exposing (Task)


type GrowthRate
    = GrowthRate { speed : GrowthRateSpeed, pokemons : List String, expPerLevel : Dict Int Int }


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


growthRateForPokemonName : String -> List GrowthRate -> Maybe GrowthRate
growthRateForPokemonName pokemonName growthRates =
    growthRates
        |> List.filter (includesPokemonName pokemonName)
        |> List.head


includesPokemonName : String -> GrowthRate -> Bool
includesPokemonName pokemonName (GrowthRate growthRate) =
    List.member pokemonName growthRate.pokemons


getGrowthRate : GrowthRateSpeed -> Task Http.Error GrowthRate
getGrowthRate speed =
    Http.task
        { method = "GET"
        , headers = []
        , url = getGrowthRateUrl (speedName speed)
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse (growthRateFromJSON speed)
        , timeout = Nothing
        }


getAllGrowthRates : Task Http.Error (List GrowthRate)
getAllGrowthRates =
    allSpeeds
        |> List.map getGrowthRate
        |> Task.sequence


growthRateFromJSON : GrowthRateSpeed -> Decoder GrowthRate
growthRateFromJSON speed =
    Json.Decode.map2 (\pokemons expPerLevel -> GrowthRate { speed = speed, pokemons = pokemons, expPerLevel = expPerLevel })
        (field "pokemon_species" (Json.Decode.list (field "name" string)))
        expPerLevelFromGrowthRateJSON


expPerLevelFromGrowthRateJSON : Decoder (Dict Int Int)
expPerLevelFromGrowthRateJSON =
    Json.Decode.map Dict.fromList <|
        field "levels" (Json.Decode.list (Json.Decode.map2 (\exp level -> ( level, exp )) (field "experience" int) (field "level" int)))
