module Core exposing (init, view, update, subscriptions)

{-| This library tries to address the following issues in building

1.  Gives default styles for all elements in an interactive virtual CS experiment.
2.  Gives readymade history support with as less code rewriting as possible using elm-community/undo-redo
3.  Extends the functionality of undo-redo package by adding support for commands and subscriptions
4.  A ready to use logger via ports which logs all states


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

1.  The library does not support Browser.Document and Browser.Application since it was not required in the present usecase.

-}

import Core.Analytics as A
import Core.Assets.Icons as I
import Core.Style as S
import Html exposing (Html)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Json.Encode as JE
import UndoList as U exposing (Msg(..), UndoList)


type DefInit
    = DefInit


type alias UndoListCmds msg =
    ( ( Cmd msg, Cmd msg ), ( Cmd msg, Cmd msg ) )


getUndoListCmds : Maybe (UndoListCmds msg) -> UndoListCmds msg
getUndoListCmds undoListCmds =
    case undoListCmds of
        Nothing ->
            ( ( Cmd.none, Cmd.none ), ( Cmd.none, Cmd.none ) )

        Just cmds ->
            cmds


multipleUndoRedo : Int -> Msg msg -> UndoList state -> UndoList state
multipleUndoRedo count wrapperMessage undolist =
    let
        newUndoList =
            case wrapperMessage of
                Redo ->
                    U.redo undolist

                Undo ->
                    U.undo undolist

                _ ->
                    undolist
    in
    if (count - 1) > 0 then
        multipleUndoRedo (count - 1) wrapperMessage newUndoList

    else
        newUndoList


{-| Update is a wrapper function which adds undo,redo,reset to an updater.
This function also logs the action and the newState via sharing it over the port.
The function takes logger, port for analytics, original update function, freshStateSetter, undoCount, undoListCommands.
The function returns a new partial function which replaces the original update in your application's main function

The details about the arguements

1.  **logger** : maps the program state to a loggable output. For no mapping _identity_ function can be used
2.  **analyticsPort** : the port through which the telemetry data needs to be shared
3.  **orginal update function** : Handlles the updates specific to your application
4.  **freshStateSetter** : A function that sets a fresh state (see undo-redo package) if the result is true.
5.  **undoCount** : Handles cases where a number of steps need to be undoed (useful when the messages are chained)
6.  **undoListCommands** : Tuple of commands which will be executed when undo/redo/reset is encountered

**NOTE** : Last three parameters are of Maybe type so you can pass Nothing if you don't wish to use them.

-}
update : (state -> loggableState) -> (JE.Value -> Cmd (Msg msg)) -> (msg -> state -> ( state, Cmd msg )) -> (msg -> Bool) -> Maybe Int -> Maybe (UndoListCmds msg) -> Msg msg -> UndoList state -> ( UndoList state, Cmd (Msg msg) )
update mapState analyticsPort updater setFresh undoRedoCount undoListCmds wrapperMessage undolist =
    let
        ( ( resetCmd, redoCmd ), ( undoCmd, forgetCmd ) ) =
            getUndoListCmds undoListCmds

        log =
            \logMsg logState -> A.sendOutLog (Just <| mapState undolist.present) logMsg (mapState logState) analyticsPort
    in
    case wrapperMessage of
        Reset ->
            let
                newUndoList =
                    U.reset undolist
            in
            ( newUndoList, Cmd.batch [ log wrapperMessage newUndoList.present, Cmd.map New resetCmd ] )

        Redo ->
            let
                newUndoList =
                    case undoRedoCount of
                        Nothing ->
                            U.redo undolist

                        Just c ->
                            multipleUndoRedo c wrapperMessage undolist
            in
            ( newUndoList, Cmd.batch [ log wrapperMessage newUndoList.present, Cmd.map New redoCmd ] )

        Undo ->
            let
                newUndoList =
                    case undoRedoCount of
                        Nothing ->
                            U.undo undolist

                        Just c ->
                            multipleUndoRedo c wrapperMessage undolist
            in
            ( newUndoList, Cmd.batch [ log wrapperMessage newUndoList.present, Cmd.map New undoCmd ] )

        Forget ->
            let
                newUndoList =
                    U.forget undolist
            in
            ( newUndoList, Cmd.batch [ log wrapperMessage newUndoList.present, Cmd.map New forgetCmd ] )

        New msg ->
            let
                ( newState, cmd ) =
                    updater msg undolist.present
            in
            if setFresh msg then
                ( U.fresh newState, Cmd.batch [ log msg newState, Cmd.map New cmd ] )

            else
                ( U.new newState undolist, Cmd.batch [ log msg newState, Cmd.map New cmd ] )


{-| Subscriptions is a wrapper function that maps the original application's subscriptions to the new types.
-}
subscriptions : (state -> Sub msg) -> UndoList state -> Sub (Msg msg)
subscriptions subscriber undolist =
    Sub.map New (subscriber undolist.present)


{-| Init is a wrapper function that maps the original application's init function to new types
It also logs the init action
-}
init : (state -> loggableState) -> (JE.Value -> Cmd (Msg msg)) -> (flags -> ( state, Cmd msg )) -> flags -> ( UndoList state, Cmd (Msg msg) )
init mapState analyticsPort initializer f =
    let
        ( newState, cmd ) =
            initializer f
    in
    ( U.fresh newState, Cmd.batch [ A.sendOutLog Nothing DefInit (mapState newState) analyticsPort, Cmd.map New cmd ] )


{-| View is the wrapper function that maps the original application's view function to new type.
It also adds the default undo/redo/reset control buttons to the UI.
-}
view : (state -> Html msg) -> UndoList state -> Html (Msg msg)
view viewer undolist =
    Html.div
        [ HA.class "experiment" ]
        [ S.style
        , Html.div [ HA.class "experiment__simulator" ] [ Html.map New (viewer undolist.present) ]
        , viewButtons undolist
        ]


viewButtons : UndoList state -> Html (Msg msg)
viewButtons undolist =
    Html.div
        [ HA.class "experiment__history" ]
        [ Html.button
            [ onClick Undo
            , HA.disabled (U.hasPast undolist |> not)
            , HA.class "button__action--secondary"
            ]
            [ I.undoIcon "black"
            , Html.text "Undo"
            ]
        , Html.button
            [ onClick Redo
            , HA.disabled (U.hasFuture undolist |> not)
            , HA.class "button__action--secondary"
            ]
            [ I.redoIcon "black"
            , Html.text "Redo"
            ]
        , Html.button
            [ onClick Reset
            , HA.disabled ((U.hasPast undolist || U.hasFuture undolist) |> not)
            , HA.class "button__action--secondary"
            ]
            [ I.resetIcon "black"
            , Html.text "Reset"
            ]
        ]
