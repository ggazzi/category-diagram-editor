module Demo.Update
    exposing
        ( Msg(..)
        , update
        , subscriptions
        )

import Demo.Model exposing (..)
import Diagram exposing (ObjectId)
import GraphView exposing (Target(..))
import Mouse.Modifiers as Mouse
import Position exposing (Position, Delta, moveBy)


-- UPDATE


type Msg
    = NoOp
    | CreateObjectAt Position
    | StartMovingObject ObjectId
    | DragBy Delta
    | DragEnd
    | GraphViewMsg GraphView.InternalMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ diagram, interaction } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CreateObjectAt { x, y } ->
            let
                newId =
                    findUniqueId model

                newObject =
                    { x = x
                    , y = y
                    , name = "X" ++ toString newId
                    }
            in
                ( { model
                    | diagram = diagram |> Diagram.insertObject newId newObject
                    , uid = newId + 1
                  }
                , Cmd.none
                )

        StartMovingObject object ->
            ( { model | interaction = MovingObject object }, Cmd.none )

        DragBy delta ->
            case interaction of
                MovingObject object ->
                    ( { model
                        | diagram = diagram |> Diagram.modifyObject object (moveBy delta)
                      }
                    , Cmd.none
                    )

                Idle ->
                    ( model, Cmd.none )

        DragEnd ->
            ( { model | interaction = Idle }, Cmd.none )

        GraphViewMsg graphViewMsg ->
            GraphView.update viewConfig graphViewMsg model


viewConfig : GraphView.Config Msg
viewConfig =
    GraphView.customConfig
        [ GraphView.onClick <|
            \{ target, modifiers, position } ->
                case ( target, Mouse.hasShift modifiers ) of
                    ( OnBackground, True ) ->
                        CreateObjectAt position

                    _ ->
                        NoOp
        , GraphView.onDragStart <|
            \{ target } ->
                case target of
                    OnNode id ->
                        StartMovingObject id

                    OnBackground ->
                        NoOp
        , GraphView.onDragBy DragBy
        , GraphView.onDragEnd DragEnd
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { graphView } =
    GraphView.subscriptions GraphViewMsg graphView
