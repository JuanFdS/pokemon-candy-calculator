module ExpCandy exposing (..)

import Html
import Html.Attributes


type alias Exp =
    Int


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


expGiven : ExpCandy -> Exp
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


imageUrl : ExpCandy -> String
imageUrl candy =
    "../assets/"
        ++ (case candy of
                CandyXS ->
                    "exp-candy-xs.png"

                CandyS ->
                    "exp-candy-s.png"

                CandyM ->
                    "exp-candy-m.png"

                CandyL ->
                    "exp-candy-l.png"

                CandyXL ->
                    "exp-candy-xl.png"
           )


candiesNeededForExp : ExpCandy -> Exp -> Int
candiesNeededForExp candy exp =
    ceiling (toFloat exp / toFloat (expGiven candy))


viewCandyNeededForExp : Exp -> ExpCandy -> Html.Html a
viewCandyNeededForExp exp candy =
    Html.h4 []
        [ viewCandyImage candy
        , Html.text <| candyName candy ++ ": " ++ String.fromInt (candiesNeededForExp candy exp)
        ]


viewCandyImage : ExpCandy -> Html.Html a
viewCandyImage candy =
    Html.img [ Html.Attributes.src (imageUrl candy) ] []
