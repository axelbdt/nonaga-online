module Nonaga exposing (..)

import Dict exposing (Dict)
import GraphicSVG as G
import Set exposing (Set)
import Types exposing (..)


initialModel : GameModel
initialModel =
    { currentPlayer = Red
    , turnPhase = MoveToken
    , board = initialBoard
    , tokens = initialTokens
    , lastMovedPlatform = ( 0, 0 )
    , selectedToken = Nothing
    , selectedPlatform = Nothing
    }


initialBoard : Board
initialBoard =
    Set.fromList
        [ ( 0, 0 )
        , ( 1, 0 )
        , ( 0, 1 )
        , ( 1, 1 )
        , ( -1, 0 )
        , ( 0, -1 )
        , ( -1, -1 )
        , ( 1, -1 )
        , ( -1, 1 )
        , ( 2, 0 )
        , ( -2, 0 )
        , ( 0, -2 )
        , ( 2, -2 )
        , ( -2, 2 )
        , ( 0, 2 )
        , ( -2, 1 )
        , ( -1, 2 )
        , ( 2, -1 )
        , ( 1, -2 )
        ]


initialTokens : Tokens
initialTokens =
    Dict.fromList
        [ ( ( 0, -2 ), Red )
        , ( ( -2, 2 ), Red )
        , ( ( 2, 0 ), Red )
        , ( ( -2, 0 ), Black )
        , ( ( 0, 2 ), Black )
        , ( ( 2, -2 ), Black )
        ]


nextPlayer : Player -> Player
nextPlayer player =
    case player of
        Red ->
            Black

        Black ->
            Red


directions : Set ( Int, Int )
directions =
    Set.fromList
        [ ( -1, 0 )
        , ( -1, 1 )
        , ( 0, 1 )
        , ( 0, -1 )
        , ( 1, -1 )
        , ( 1, 0 )
        ]


platformAt : Board -> Platform -> Bool
platformAt board platform =
    Set.member platform board


neighbors : Platform -> Set Platform
neighbors ( x, y ) =
    Set.map (\( dx, dy ) -> ( x + dx, y + dy )) directions


countNeighboringPlatforms : Board -> Platform -> Int
countNeighboringPlatforms board platform =
    neighbors platform
        |> Set.filter (platformAt board)
        |> Set.size


isPlatformDestination : Board -> Platform -> Platform -> Bool
isPlatformDestination board selected platform =
    let
        neighborCount =
            countNeighboringPlatforms (Set.remove selected board) platform
    in
    not (Set.member platform board) && neighborCount >= 2 && neighborCount < 5


findPlatformDestinations : Board -> Platform -> Set Platform
findPlatformDestinations board platform =
    Set.toList board
        |> List.map neighbors
        |> List.foldl Set.union Set.empty
        |> Set.filter (isPlatformDestination board platform)
        |> Set.remove platform


tokenAt : Tokens -> Platform -> Bool
tokenAt tokens platform =
    Dict.member platform tokens


platformIsSelectable : Board -> Tokens -> Platform -> Bool
platformIsSelectable board tokens platform =
    countNeighboringPlatforms board platform < 5 && not (tokenAt tokens platform)


checkDirection : Board -> Tokens -> Platform -> ( Int, Int ) -> Platform
checkDirection board tokens start direction =
    let
        ( x, y ) =
            start

        checkedPosition =
            ( x + Tuple.first direction, y + Tuple.second direction )
    in
    if platformAt board checkedPosition && not (tokenAt tokens checkedPosition) then
        checkDirection board tokens checkedPosition direction

    else
        start


findTokenDestinations : Board -> Tokens -> Platform -> Set Platform
findTokenDestinations board tokens token =
    Set.map (checkDirection board tokens token) directions
        |> Set.remove token


moveToken : Tokens -> Platform -> Platform -> Tokens
moveToken tokens from to =
    case Dict.get from tokens of
        Just player ->
            tokens
                |> Dict.remove from
                |> Dict.insert to player

        Nothing ->
            tokens


checkWinner : Player -> Tokens -> Bool
checkWinner player tokens =
    let
        platforms =
            Dict.filter (\_ p -> p == player) tokens
                |> Dict.keys
    in
    List.any (tokenIsWinner (Set.fromList platforms)) platforms


tokenIsWinner : Set Platform -> Platform -> Bool
tokenIsWinner tokens token =
    tokens
        |> Set.intersect (neighbors token)
        |> Set.size
        |> (==) 2


update : GameMsg -> GameModel -> GameModel
update msg model =
    case msg of
        SelectToken player token ->
            case model.turnPhase of
                MoveToken ->
                    if model.currentPlayer == player then
                        { model | selectedToken = Just token }

                    else
                        model

                MovePlatform ->
                    model

        ChooseTokenDestination from to ->
            let
                newTokens =
                    moveToken model.tokens from to
            in
            { model
                | turnPhase = MovePlatform
                , tokens = newTokens
                , selectedToken = Nothing
            }

        SelectPlatform platform ->
            case model.turnPhase of
                MovePlatform ->
                    if
                        not (checkWinner model.currentPlayer model.tokens)
                            && platformIsSelectable model.board model.tokens platform
                            && (platform /= model.lastMovedPlatform)
                    then
                        { model | selectedPlatform = Just platform }

                    else
                        model

                MoveToken ->
                    model

        ChoosePlatformDestination from to ->
            let
                newBoard =
                    model.board |> Set.remove from |> Set.insert to
            in
            { model
                | currentPlayer = nextPlayer model.currentPlayer
                , turnPhase = MoveToken
                , lastMovedPlatform = to
                , selectedPlatform = Nothing
                , board = newBoard
            }

        Reset ->
            initialModel


view : GameModel -> List (G.Shape FrontendMsg)
view model =
    [ boardView model.board model.selectedPlatform
    , G.group
        (case model.selectedPlatform of
            Nothing ->
                []

            Just selected ->
                findPlatformDestinations model.board selected
                    |> Set.toList
                    |> List.map (platformDestinationView selected)
        )
    , tokensView
        model.selectedToken
        model.tokens
    , G.group
        (case model.selectedToken of
            Nothing ->
                []

            Just selected ->
                findTokenDestinations model.board model.tokens selected
                    |> Set.toList
                    |> List.map (tokenDestinationView model.currentPlayer selected)
        )
    , if checkWinner model.currentPlayer model.tokens then
        winnerView model.currentPlayer

      else
        G.group []
    ]


placeShape : ( Int, Int ) -> G.Shape FrontendMsg -> G.Shape FrontendMsg
placeShape ( x, y ) shape =
    shape
        |> G.move ( 100 * (toFloat x + cos (pi / 3) * toFloat y), 100 * sin (pi / 3) * toFloat y )


platformColor : Bool -> G.Color
platformColor selected =
    if selected then
        G.lightYellow

    else
        G.yellow


platformShape : Bool -> G.Shape FrontendMsg
platformShape selected =
    G.circle 50
        |> G.filled (platformColor selected)


platformView : Maybe Platform -> Platform -> G.Shape FrontendMsg
platformView selectedPlatform platform =
    (case selectedPlatform of
        Nothing ->
            platformShape False

        Just selected ->
            platformShape (selected == platform)
    )
        |> placeShape platform
        |> G.notifyTap (GameMsg (SelectPlatform platform))


boardView : Board -> Maybe Platform -> G.Shape FrontendMsg
boardView board selectedPlatform =
    Set.toList board
        |> List.map (platformView selectedPlatform)
        |> G.group


platformDestinationView : Platform -> Platform -> G.Shape FrontendMsg
platformDestinationView from to =
    platformShape False
        |> placeShape to
        |> G.makeTransparent 0.6
        |> G.notifyTap (GameMsg (ChoosePlatformDestination from to))


tokenColor : Player -> Bool -> G.Color
tokenColor player selected =
    case player of
        Red ->
            if selected then
                G.lightRed

            else
                G.red

        Black ->
            if selected then
                G.charcoal

            else
                G.darkCharcoal


tokenShape : Player -> Bool -> G.Shape FrontendMsg
tokenShape player selected =
    G.circle 40 |> G.filled (tokenColor player selected)


tokenView : Maybe Platform -> Platform -> Player -> G.Shape FrontendMsg
tokenView selectedToken platform player =
    (case selectedToken of
        Nothing ->
            tokenShape player False

        Just selected ->
            tokenShape player (selected == platform)
    )
        |> placeShape platform
        |> G.notifyTap (GameMsg (SelectToken player platform))


tokensView : Maybe Platform -> Tokens -> G.Shape FrontendMsg
tokensView selectedToken tokens =
    Dict.map (tokenView selectedToken) tokens
        |> Dict.values
        |> G.group


tokenDestinationView : Player -> Platform -> Platform -> G.Shape FrontendMsg
tokenDestinationView player from to =
    tokenShape player False
        |> placeShape to
        |> G.makeTransparent 0.6
        |> G.notifyTap (GameMsg (ChooseTokenDestination from to))


winnerView : Player -> G.Shape FrontendMsg
winnerView player =
    G.group
        [ G.roundedRect 240 120 5 |> G.filled G.white
        , G.text (playerText player ++ " wins!") |> G.centered |> G.size 32 |> G.filled (tokenColor player False) |> G.move ( 0, 16 )
        , G.group
            [ G.roundedRect 96 48 5 |> G.filled (tokenColor player False)
            , G.text "Retry" |> G.centered |> G.size 24 |> G.filled G.white |> G.move ( 0, -8 )
            ]
            |> G.move ( 0, -24 )
            |> G.notifyTap (GameMsg Reset)
        ]


playerText : Player -> String
playerText player =
    case player of
        Red ->
            "Red"

        Black ->
            "Black"