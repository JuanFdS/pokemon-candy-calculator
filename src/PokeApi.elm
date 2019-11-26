module PokeApi exposing (..)


pokeApiUrl : String
pokeApiUrl =
    "https://pokeapi.co/api/v2/"


getPokemonUrl : String -> String
getPokemonUrl pokemonName =
    pokeApiUrl ++ "pokemon/" ++ pokemonName


getGrowthRateUrl : String -> String
getGrowthRateUrl growthRateSpeedName =
    pokeApiUrl ++ "growth-rate/" ++ growthRateSpeedName
