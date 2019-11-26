module Pokemon exposing (Pokemon(..), pokemonFromJSON)

import GrowthRate exposing (GrowthRate, growthRateForPokemonName)
import Json.Decode exposing (Decoder, at, field, string)


type Pokemon
    = Pokemon
        { imageUrl : String
        , name : String
        }


pokemonFromJSON : List GrowthRate -> Decoder Pokemon
pokemonFromJSON growthRates =
    Json.Decode.map2 (\imageUrl name -> Pokemon { imageUrl = imageUrl, name = name })
        (at [ "sprites", "front_default" ] string)
        (field "name" string)
