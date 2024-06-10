module DecisionTree exposing (DecisionTree, Choice(..), choose, flatten, empty, matches)

type Choice
    = HasPongTable Bool
    | HasPongBalls Bool
    | CupCount Int
    | HasCoin Bool
    | HasCardBoardBox Bool
    | HasCards Bool
    | EveryonePlays Bool
    -- ...

type Question
    = HasPongTableQ
    | HasPongBallsQ
    | CupCountQ
    | HasCoinQ
    | HasCardBoardBoxQ
    | HasCardsQ
    | EveryonePlaysQ


getQuestion : Choice -> Question
getQuestion choice =
    case choice of
        HasPongTable _ ->
            HasPongTableQ

        HasPongBalls _ ->
            HasPongBallsQ

        CupCount _ ->
            CupCountQ

        HasCoin _ ->
            HasCoinQ

        HasCardBoardBox _ ->
            HasCardBoardBoxQ

        HasCards _ ->
            HasCardsQ

        EveryonePlays _ ->
            EveryonePlaysQ


-- does the game satisfy the user's choices?
satisfies : Choice -> Choice -> Bool
satisfies userChoice gameAttribute =
    case (userChoice, gameAttribute) of
        (HasPongTable u, HasPongTable g) ->
            u == g -- could get fancier here to not rule out non-table games if user has a table

        (HasPongBalls u, HasPongBalls g) ->
            u == g

        (CupCount u, CupCount g) ->
            u >= g

        (HasCoin u, HasCoin g) ->
            u == g

        (HasCardBoardBox u, HasCardBoardBox g) ->
            u == g

        (HasCards u, HasCards g) ->
            u == g

        (EveryonePlays u, EveryonePlays g) ->
            u == g

        (_, _) ->
            False -- we should never get here




type DecisionTree 
    = DTree (List (Question, Choice))

empty : DecisionTree
empty =
    DTree []

choose : DecisionTree -> Choice -> DecisionTree
choose (DTree tree) answer =
    let
        question = getQuestion answer
    in
    DTree (chooseList tree question answer)

chooseList : List (Question, Choice) -> Question -> Choice -> List (Question, Choice)
chooseList tree question answer =
    case tree of
        [] ->
            [ (question, answer) ]

        (nodeQ, nodeA) :: subtree ->
            if nodeQ == question && nodeA == answer then
                tree -- nothing changed

            else if nodeQ == question then
               [ (question, answer) ] -- answer changed, drop all child answers

            else
               (nodeQ, nodeA) :: chooseList subtree question answer

flatten : DecisionTree -> List Choice
flatten (DTree tree) =
    List.map (\(_, a) -> a) tree


matches : DecisionTree -> List Choice -> Bool
matches tree attributes =
    case tree of
        DTree [] ->
            True

        DTree (node :: subtree) ->
            matchesHelp node subtree attributes

matchesHelp : (Question, Choice) -> List (Question, Choice) -> List Choice -> Bool
matchesHelp node tree attributes =
    case nodeMatches node attributes of
        Just False ->
            False

        _ -> -- Just True | Nothing (rest of tree must match too)
            case tree of
                [] ->
                    True
           
                head :: tail ->
                    matchesHelp head tail attributes

nodeMatches : (Question, Choice) -> List Choice -> Maybe Bool
nodeMatches (targetQ, targetA) attrs =
    case attrs of
        [] ->
            Nothing

        headA :: tail ->
            let
                headQ = getQuestion headA
            in
            if headQ == targetQ then
                Just <| satisfies targetA headA
            else
                nodeMatches (targetQ, targetA) tail



