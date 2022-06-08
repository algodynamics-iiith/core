module DrivingTest exposing (MsgType(..), State, primaryButton, sandbox)

{-| This library tries to address the following issues in building

1.  Gives default styles for all elements in an interactive virtual CS experiment.
2.  Gives readymade history support with as less code rewriting as possible using elm-community/undo-redo
3.  Extends the functionality of undo-redo package by adding support for commands and subscriptions
4.  A ready to use logger via ports which logs all states


## Conventions

1.  Transition System (ts) : refers to the transition system you wish to visualise
2.  UI : refers to the additional state in the application that must be held to implement transition system.
3.  Experiment : refers to the Driver Transition System, which has the complete states etc.

For learning about how to convert and algorithmic transition system to Driver version please see this [link]()

The backend logs all actions even including meta action like Undo/Redo/Reset/Submit,
as well as both UI actions


# Wrapper Functions

@docs init, view, update, subscriptions


# Example

A very basic example which should suit most basic cases

    -- import the package into your app and update the main function with the following
    Browser.element

    { init = Core.init identity analyticsPort init
    , view = Core.view view
    , update = Core.update identity analyticsPort update setFresh Nothing Nothing
    , subscriptions = Core.subscriptions subscriptions
    }

For using advanced features visit the Readme.md file.


# Limitations

1.  The library does not support Browser.element, Browser.Document and Browser.Application
    since it was not required in the present usecase. Hence, algorithms like Randomized quicksort
    can not be implemented.

-}

import Browser
import Core.Analytics as A
import Core.Assets.Icons as I
import Core.Prompt as P
import Core.Style as S
import EditDistance as ED
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Encode as JE
import List.Extra as LE
import UndoList as U exposing (UndoList)



{-
   Transition System : State Space
   Elm : Model

-}


type alias State ui ts =
    { ui : ui
    , ts : ts
    }


type alias Experiment ui ts msg =
    UndoList
        { state : State ui ts
        , prompt : P.Prompt
        , debt : Int
        , user : List msg
        , correct : List (List msg)
        , score : Score
        }



-- type alias UIModel =
--     { i : Int
--     , j : Int
--     }
-- type Msg
--     = Select Int
--     | Swap Int Int


type MsgType
    = UIMsg
    | TSMsg


type Msg a
    = Reset
    | Redo
    | Undo
    | Forget
    | Submit
    | ConfirmSubmit
    | CancelSubmit
    | New a
    | Init a


type Score
    = NotSubmitted
    | WaitingConfirmation
    | Scored Float


sandbox :
    { init :
        ()
        -> ( State ui ts, Cmd msg ) -- default Initialisation value and a random initialisation command
    , view :
        State ui ts
        -> Html msg -- h function of the transition system
    , btns : List (PrimaryButton msg)
    , update :
        msg
        -> State ui ts
        -> State ui ts -- transition relation of the algorithm
    , isEnabled :
        msg
        -> State ui ts
        -> Result String String -- Check if the given action is enabled on given state
    , next :
        State ui ts
        -> List msg -- For a given state what are possible next actions
    , msgType :
        msg
        -> MsgType -- checks if a transition changes ui or ts
    , analyticsPort :
        JE.Value
        -> Cmd msg -- for sending data out to the js world
    }
    -> Program () (Experiment ui ts msg) (Msg msg)
sandbox impl =
    Browser.element
        { init = init impl.init
        , view = view impl.btns impl.view
        , update = update impl.update impl.isEnabled impl.next impl.msgType impl.analyticsPort
        , subscriptions = \_ -> Sub.none
        }



-- I N T E R N A L  T Y P E S


update :
    (msg -> State ui ts -> State ui ts)
    -> (msg -> State ui ts -> Result String String)
    -> (State ui ts -> List msg)
    -> (msg -> MsgType)
    -> (JE.Value -> Cmd msg)
    -> Msg msg
    -> Experiment ui ts msg
    -> ( Experiment ui ts msg, Cmd (Msg msg) )
update updater isEnabled next msgType analyticsPort wrapperMessage exp =
    let
        log =
            \logMsg logState -> A.sendOutLog (Just <| exp.present) logMsg logState analyticsPort |> Cmd.map New
    in
    case exp.present.score of
        NotSubmitted ->
            -- Machine free, available for interaction by user.
            case wrapperMessage of
                Reset ->
                    let
                        newExp =
                            U.reset exp
                    in
                    ( newExp, log wrapperMessage newExp.present )

                Redo ->
                    let
                        newExp =
                            U.redo exp
                    in
                    ( newExp, log wrapperMessage newExp.present )

                Undo ->
                    let
                        newExp =
                            U.undo exp
                    in
                    ( newExp, log wrapperMessage newExp.present )

                Forget ->
                    let
                        newExp =
                            U.forget exp
                    in
                    ( newExp, log wrapperMessage newExp.present )

                Submit ->
                    let
                        present =
                            exp.present

                        newExp =
                            { present | score = WaitingConfirmation }
                    in
                    ( U.new newExp exp, log wrapperMessage newExp )

                Init msg ->
                    let
                        ( prompt, debt ) =
                            case isEnabled msg exp.present.state of
                                Ok str ->
                                    if exp.present.debt == 0 then
                                        ( ( str, P.PromptSuccess ), exp.present.debt )

                                    else
                                        ( ( str, P.PromptSuccess ), exp.present.debt + 1 )

                                Err str ->
                                    ( ( str, P.PromptDanger ), exp.present.debt + 1 )

                        state =
                            updater msg exp.present.state

                        user =
                            case msg |> msgType of
                                TSMsg ->
                                    List.append exp.present.user [ msg ]

                                UIMsg ->
                                    exp.present.user

                        newExp =
                            { state = state
                            , prompt = prompt
                            , debt = debt
                            , correct = exp.present.correct
                            , user = user
                            , score = exp.present.score
                            }
                    in
                    ( U.fresh newExp, log wrapperMessage newExp )

                New msg ->
                    let
                        ( prompt, debt ) =
                            case isEnabled msg exp.present.state of
                                Ok str ->
                                    if exp.present.debt == 0 then
                                        ( ( str, P.PromptSuccess ), exp.present.debt )

                                    else
                                        ( ( str, P.PromptSuccess ), exp.present.debt + 1 )

                                Err str ->
                                    ( ( str, P.PromptDanger ), exp.present.debt + 1 )

                        state =
                            updater msg exp.present.state

                        user =
                            case msg |> msgType of
                                TSMsg ->
                                    List.append exp.present.user [ msg ]

                                UIMsg ->
                                    exp.present.user

                        newExp =
                            { state = state
                            , prompt = prompt
                            , debt = debt
                            , correct = exp.present.correct
                            , user = user
                            , score = exp.present.score
                            }
                    in
                    ( U.new newExp exp, log msg newExp )

                _ ->
                    -- No other action (Confirm Submit and Cancel Submit allowed)
                    ( exp, Cmd.none )

        WaitingConfirmation ->
            -- Machine partially locked, waiting for user input
            case wrapperMessage of
                CancelSubmit ->
                    let
                        present =
                            exp.present

                        newExp =
                            { present | score = NotSubmitted }
                    in
                    ( U.new newExp exp, log wrapperMessage newExp )

                ConfirmSubmit ->
                    let
                        startState =
                            U.reset exp

                        correctRuns =
                            generateCorrectRuns next msgType updater ( startState.present.state, [] )

                        minED =
                            List.map (\cs -> ( ED.levenshtein exp.present.user cs, cs )) correctRuns
                                |> LE.minimumBy Tuple.first
                                |> Maybe.withDefault ( 0, [] )

                        ( d, n ) =
                            ( Tuple.first minED, Tuple.second minED |> List.length )

                        score =
                            Debug.log "Score" ((toFloat (n - d) / toFloat n) * 100 |> Scored)

                        present =
                            exp.present

                        newExp =
                            { present | score = score }
                    in
                    ( U.new newExp exp, log wrapperMessage newExp )

                _ ->
                    ( exp, Cmd.none )

        Scored _ ->
            -- Machine locked => No more actions allowed
            ( exp, Cmd.none )


{-| Init is a wrapper function that maps the original application's init function to new types
It also logs the init action
-}
init : (() -> ( State ui ts, Cmd msg )) -> flags -> ( Experiment ui ts msg, Cmd (Msg msg) )
init initializer _ =
    let
        ( state, cmd ) =
            initializer ()

        newExp =
            { state = state, prompt = ( " ", P.PromptInfo ), debt = 0, correct = [], user = [], score = NotSubmitted }
    in
    ( U.fresh newExp, Cmd.map Init cmd )


{-| View is the wrapper function that maps the original application's view function to new type.
It also adds the default undo/redo/reset control buttons to the UI.
-}
view : List (PrimaryButton msg) -> (State ui ts -> Html msg) -> Experiment ui ts msg -> Html (Msg msg)
view btns viewer exp =
    Html.div
        [ HA.class "experiment" ]
        [ S.style
        , Html.div [ HA.class "experiment__simulator" ]
            [ Html.div [ HA.class "experiment-container" ]
                [ viewFeedback exp.present.prompt
                , Html.div
                    [ HA.class "observables-container" ]
                    [ Html.map New (viewer exp.present.state)
                    ]
                , viewPrimaryButtons btns
                ]
            ]
        , viewMetaButtons exp
        , viewScore exp.present.score
        ]


viewMetaButtons : Experiment ui ts msg -> Html (Msg msg)
viewMetaButtons exp =
    Html.div
        [ HA.class "experiment__history" ]
        [ Html.button
            [ onClick Undo
            , HA.disabled (U.hasPast exp |> not)
            , HA.class "button__action--secondary"
            ]
            [ I.undoIcon "black"
            , Html.text "Undo"
            ]
        , Html.button
            [ onClick Redo
            , HA.disabled (U.hasFuture exp |> not)
            , HA.class "button__action--secondary"
            ]
            [ I.redoIcon "black"
            , Html.text "Redo"
            ]
        , Html.button
            [ onClick Reset
            , HA.disabled ((U.hasPast exp || U.hasFuture exp) |> not)
            , HA.class "button__action--secondary"
            ]
            [ I.resetIcon "black"
            , Html.text "Reset"
            ]
        , Html.button
            [ onClick Submit
            , HA.class "button__action--secondary"
            ]
            [ I.submitIcon "black"
            , Html.text "Submit"
            ]
        ]


viewFeedback : P.Prompt -> Html (Msg msg)
viewFeedback prompt =
    let
        ( promptText, promptType ) =
            prompt

        class =
            case promptType of
                P.PromptDanger ->
                    "prompt--danger"

                P.PromptInfo ->
                    "prompt--info"

                P.PromptSuccess ->
                    "prompt--success"
    in
    Html.div
        [ HA.class "feedback-container" ]
        [ Html.div [ HA.class class ] [ Html.text promptText ]
        ]


viewPrimaryButtons : List (PrimaryButton msg) -> Html (Msg msg)
viewPrimaryButtons btns =
    Html.map New <|
        Html.div
            [ HA.class "controls-container" ]
            btns


viewScore : Score -> Html (Msg msg)
viewScore score =
    case score of
        NotSubmitted ->
            Html.div [ HA.style "display" "none" ] []

        WaitingConfirmation ->
            Html.div
                [ HA.class "modal-bg"
                ]
                [ Html.div
                    [ HA.class "modal-encl" ]
                    [ Html.div
                        [ HA.class "modal-content" ]
                        [ Html.div [ HA.class "modal-header" ]
                            [ Html.h3 [ HA.style "margin" "0px" ] [ Html.text (String.fromChar (Char.fromCode 9888) ++ " Warning") ]
                            , Html.button [ HA.style "cursor" "pointer", onClick CancelSubmit ] [ Html.text (String.fromChar (Char.fromCode 10005)) ]
                            ]
                        , Html.div [ HA.class "modal-body" ]
                            [ Html.text "Are you sure you want to submit?" ]
                        , Html.div [ HA.class "modal-body" ]
                            [ Html.button [ HA.class "button__action--primary", onClick ConfirmSubmit ] [ Html.text "Finish And Submit" ]
                            , Html.button [ HA.class "button__action--secondary", onClick CancelSubmit ] [ Html.text "Cancel" ]
                            ]
                        ]
                    ]
                ]

        Scored s ->
            Html.div
                [ HA.class "modal-bg"
                ]
                [ Html.div
                    [ HA.class "modal-encl" ]
                    [ Html.div
                        [ HA.class "modal-content" ]
                        [ Html.div [ HA.class "modal-header" ]
                            [ Html.h3 [ HA.style "margin" "0px" ] [ Html.text "Driving Test Score" ]
                            , Html.button [ HA.style "cursor" "pointer" ] [ Html.text (String.fromChar (Char.fromCode 10005)) ]
                            ]
                        , Html.div [ HA.class "modal-body" ]
                            [ Html.text ("Score : " ++ String.fromFloat s) ]
                        ]
                    ]
                ]



-- Internal Helpers


generateCorrectRuns : (State ui ts -> List msg) -> (msg -> MsgType) -> (msg -> State ui ts -> State ui ts) -> ( State ui ts, List msg ) -> List (List msg)
generateCorrectRuns next msgType updater ( curState, cs ) =
    case
        next curState
            |> List.filter
                (msgType
                    >> (==) TSMsg
                )
    of
        [] ->
            [ cs ]

        ms ->
            List.map
                (\m -> generateCorrectRuns next msgType updater ( updater m curState, List.append cs [ m ] ))
                ms
                |> List.concat


type alias PrimaryButton msg =
    Html msg


primaryButton : String -> msg -> PrimaryButton msg
primaryButton label msg =
    Html.button
        [ onClick msg
        , HA.class "button__action--primary"
        ]
        [ Html.text label ]
