module ExpCandy exposing (..)


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


candiesNeededForExp : ExpCandy -> Exp -> Int
candiesNeededForExp candy exp =
    ceiling (toFloat exp / toFloat (expGiven candy))
