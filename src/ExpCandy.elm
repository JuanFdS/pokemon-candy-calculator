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


type alias CandiesForExp =
    { xs : Int, s : Int, m : Int, l : Int, xl : Int }


allCandiesNeededForExp : Exp -> CandiesForExp
allCandiesNeededForExp exp =
    { xs = candiesNeededForExp CandyXS exp, s = candiesNeededForExp CandyS exp, m = candiesNeededForExp CandyM exp, l = candiesNeededForExp CandyL exp, xl = candiesNeededForExp CandyXL exp }


viewCandy : ExpCandy -> Html.Html a
viewCandy candy =
    Html.div [ style "display" "flex", style "align-items" "center", style "flex-direction" "row", style "margin-right" "1.5em", style "margin-left" "1.5em" ]
        [ Html.h4 [] [ Html.text <| candyName candy ]
        , viewCandyImage candy
        ]


viewAmountofCandyNeededForExp : Exp -> ExpCandy -> Html.Html a
viewAmountofCandyNeededForExp exp candy =
    let
        candiesNeeded =
            candiesNeededForExp candy exp

        candiesNeededText =
            if candiesNeeded <= 0 then
                "--"

            else
                String.fromInt candiesNeeded
    in
    Html.h4 [] [ Html.text candiesNeededText ]


viewCandyImage : ExpCandy -> Html.Html a
viewCandyImage candy =
    Html.img [ style "width" "3em", style "height" "3em", Html.Attributes.src (imageUrl candy) ] []
