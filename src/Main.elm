module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Events exposing (onClick)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import DecisionTree exposing (DecisionTree, Choice(..))


main =
        Browser.sandbox { init = initModel, update = update, view = view }

-- MODEL

type alias Model =
    { decisionTree : DecisionTree 
    , gameOptions : List Game
    , questions : List Question
    }

type alias Game =
    { name : String
    , attributes : List Choice
    }

type alias Question =
    { text : String
    , answers : List (String, Choice)
    , dependencies : List Choice
    }

initModel = 
    { decisionTree = DecisionTree.empty 
    , gameOptions = allGames
    , questions = List.take 1 allQuestions
    }

allGames = 
    [ 
        { name = "Pong"
        , attributes = [ HasPongTable True, HasPongBalls True, CupCount 20, EveryonePlays False ]
        }
    ,   { name = "Flip Cup"
        , attributes = [ HasPongTable True, HasPongBalls False, CupCount 6, EveryonePlays True ]
        }

    ,   { name = "Big Pong (21-Cup)"
        , attributes = [ HasPongTable True, HasPongBalls True, CupCount 42, EveryonePlays False ]
        }
    ,   { name = "Civil War"
        , attributes = [ HasPongTable True, HasPongBalls True, CupCount 13, EveryonePlays False ]
        }
    ,   { name = "Fuck Yeah"
        , attributes = [ HasPongTable True, HasPongBalls False, CupCount 8, EveryonePlays True ]
        }
    ,   { name = "Baseball"
        , attributes = [ HasPongTable True, HasPongBalls True, CupCount 10, EveryonePlays True ]
        }
    ,   { name = "Stack Cup (Rage Cage)"
        , attributes = [ HasPongTable True, HasPongBalls True, CupCount 20, EveryonePlays True ]
        }
    ,   { name = "Pizza Box"
        , attributes = [ HasPongTable False, HasCoin True, HasCardBoardBox True, HasCards False ]
        }
    ,   { name = "Ride the Bus"
        , attributes = [ HasPongTable False, HasCards True, PayAttention False ]
        }
    ,   { name = "Fuck the Dealer"
        , attributes = [ HasPongTable False, HasCards True, PayAttention False ]
        }
    ,   { name = "Quarters"
        , attributes = [ HasPongTable False, HasCoin True, HasCards False, HasCardBoardBox False ]
        }
    ,   { name = "Cheers, Governor"
        , attributes = [ HasPongTable False, HasCards False, HasCardBoardBox False, HasCoin False ]
        }
    ,   { name = "King's Cup (Kings)"
        , attributes = [ HasPongTable False, HasCards True, HasCardBoardBox False, HasCoin False, PayAttention True ]
        }
    ,   { name = "Box Game"
        , attributes = [ HasPongTable False, HasCards False, HasCardBoardBox True, HasCoin False ]
        }
    ]

allQuestions : List Question 
allQuestions = 
    [   { text = "Do you have a ping pong table or similar long table?"
        , answers = [ ("Yes", HasPongTable True), ("No", HasPongTable False) ]
        , dependencies = []
        }
    ,   { text = "Do you want to take turns or have everyone play at once?"
        , answers = [ ("Take turns", EveryonePlays False), ("Everyone at once!", EveryonePlays True) ]
        , dependencies = [ HasPongTable True ]
        }
    ,   { text = "Do you have at least two ping pong balls?"
        , answers = [ ("Yes", HasPongBalls True), ("No", HasPongBalls False) ]
        , dependencies = [ HasPongTable True ]
        }
    ,   { text = "How many Red Solo cups do you have?"
        , answers = [ ("None :(", CupCount 0), ("Like, 8, maybe", CupCount 8), ("At least 15, for sure", CupCount 15), ("20, but no more", CupCount 20), ("More than you could ever dream of", CupCount 100) ]
        , dependencies = [ HasPongTable True ]
        }
    ,   { text = "Do you have a deck of cards?"
        , answers = [ ("Yes", HasCards True), ("No", HasCards False) ]
        , dependencies = [ HasPongTable False ]
        }
    ,   { text = "Do you have a cardboard box?"
        , answers = [ ("Yes", HasCardBoardBox True), ("No", HasCardBoardBox False) ]
        , dependencies = [ HasPongTable False, HasCards False ]
        }
    ,   { text = "Do you have a coin?"
        , answers = [ ("Yes", HasCoin True), ("No", HasCoin False) ]
        , dependencies = [ HasPongTable False, HasCards False ]
        }
    ,   { text = "Are most of your friends willing and able to pay attention to the game or are there going to be a lot of side conversations going on?"
        , answers = [ ("We will give this game our undivided attention", PayAttention True), ("It's chaos out here", PayAttention False) ]
        ,  dependencies = [ HasPongTable False, HasCards True ]
        }
    ]

-- MSG

type Msg 
    = Choose Choice
    | Reset

update msg model =
    case msg of
        Choose choice ->
            let
                newTree = DecisionTree.choose model.decisionTree choice
                newGames = List.filter (\g -> DecisionTree.matches newTree g.attributes) allGames
                newQuestions = List.filter (\q -> DecisionTree.matches newTree q.dependencies) allQuestions
            in
            { model | decisionTree = newTree
                    , gameOptions = newGames
                    , questions = newQuestions
            }
    
        Reset ->
            { model | decisionTree = DecisionTree.empty, gameOptions = allGames, questions = List.take 1 allQuestions }

-- VIEW

view model =
    layout
        []
        (column 
            [ spacing 24, padding 12 ]
            ([ row 
                [ width fill ]
                [ el [ Region.heading 1, Font.size 24 ] <| text "What drinking game should we play?"
                , Input.button [ alignRight, Border.width 1, padding 6 ] 
                    { onPress = Just Reset, label = text "Reset" }
                ]
            ] ++
            (List.map (viewQuestion model) model.questions) ++
            [ paragraph [] [ text (String.fromInt (List.length model.gameOptions) ++ " result(s)") ]
            , case model.gameOptions of
                [] ->
                    paragraph [] [ text "No games for you. Try different answers or play my favorite game: chug, chug, chug!" ]
                _ ->
                    column [ spacing 12, width fill, padding 8 ]
                        (List.map viewGameCard model.gameOptions)
            ])
        )

viewQuestion : Model -> Question -> Element Msg
viewQuestion { decisionTree } question =
    column
        [ spacing 12 ]
        [ el [ paddingXY 12 0, Font.size 18 ] <| text question.text
        , row [ spacing 8, width fill, paddingXY 12 6 ]
            (List.map
                (\(ansLabel, ansChoice) -> answerButton { label = ansLabel, choice = ansChoice, decisionTree = decisionTree })
                question.answers
            )
        ]

answerButton : { label : String, choice : Choice, decisionTree : DecisionTree } -> Element Msg
answerButton { label, choice, decisionTree } =
    let
        selected = List.member choice (DecisionTree.flatten decisionTree)
    in
    Input.button
        [ Border.width 1
        , padding 4
        , Background.color (if selected then rgb255 50 180 230 else rgb255 250 250 250)
        ]
        { label = text label
        , onPress = Just <| Choose choice
        }




viewGameCard game =
    paragraph
        []
        [ el [ paddingXY 4 0 ] (text "-")
        , el [] (text game.name)
        ]

