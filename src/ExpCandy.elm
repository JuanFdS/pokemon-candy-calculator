module ExpCandy exposing (..)

import Html
import Html.Attributes exposing (style)


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
            "XS"

        CandyS ->
            "S"

        CandyM ->
            "M"

        CandyL ->
            "L"

        CandyXL ->
            "XL"


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
    Html.h4 [ style "display" "flex", style "flex-direction" "column", style "margin-right" "1.5em", style "margin-left" "1.5em" ]
        [ Html.text <| candyName candy
        , viewCandyImage candy
        , Html.text <| String.fromInt (candiesNeededForExp candy exp)
        ]


viewCandyImage : ExpCandy -> Html.Html a
viewCandyImage candy =
    Html.img [ style "width" "3em", style "height" "3em", Html.Attributes.src (imageUrl candy) ] []
