module Demo.Update
    exposing
        ( Msg(..)
        , update
        , subscriptions
        , graphViewConfig
        )

import Demo.Model exposing (..)
import Diagram exposing (ObjectId)
import GraphView exposing (Target(..))
import Json.Decode as Json
import Mouse.Modifiers as Mouse
import Position exposing (Position, Delta, moveBy)


-- UPDATE


type Msg
    = NoOp
      -- Creation of objects/morphisms
    | CreateObjectAt Position
    | StartCreatingMorphismFrom ObjectId
    | CreateMorphismTo ObjectId
      -- Drag-related events
    | StartMovingObject ObjectId
    | DragBy Delta
    | DragEnd
    | GraphViewMsg GraphView.InternalMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ diagram, interaction } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- Creation of objects/morphisms
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

        StartCreatingMorphismFrom objectId ->
            case diagram |> Diagram.getObject objectId of
                Nothing ->
                    ( model, Cmd.none )

                Just { x, y } ->
                    ( { model
                        | interaction = CreatingMorphismFrom objectId { x = x, y = y }
                      }
                    , Cmd.none
                    )

        CreateMorphismTo codomainId ->
            case interaction of
                CreatingMorphismFrom domainId _ ->
                    ( { model
                        | diagram = diagram |> Diagram.insertMorphism ( domainId, codomainId ) { name = "" }
                        , interaction = Idle
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        -- Drag-related events
        StartMovingObject objectId ->
            ( { model | interaction = MovingObject objectId }, Cmd.none )

        DragBy delta ->
            case interaction of
                MovingObject objectId ->
                    ( { model
                        | diagram = diagram |> Diagram.modifyObject objectId (moveBy delta)
                      }
                    , Cmd.none
                    )

                CreatingMorphismFrom domainId position ->
                    ( { model
                        | interaction = CreatingMorphismFrom domainId (position |> moveBy delta)
                      }
                    , Cmd.none
                    )

                Idle ->
                    ( model, Cmd.none )

        DragEnd ->
            ( { model | interaction = Idle }, Cmd.none )

        GraphViewMsg graphViewMsg ->
            GraphView.update graphViewConfig graphViewMsg model


graphViewConfig : GraphView.Config Msg
graphViewConfig =
    GraphView.customConfig
        [ GraphView.onMouseUp <|
            \target ->
                case target of
                    OnBackground ->
                        Nothing

                    OnNode id ->
                        Just (Json.succeed (CreateMorphismTo id))
        , GraphView.onClick <|
            \{ target, modifiers, position } ->
                case ( target, Mouse.hasShift modifiers ) of
                    ( OnBackground, True ) ->
                        CreateObjectAt position

                    _ ->
                        NoOp
        , GraphView.onDragStart <|
            \{ target, modifiers } ->
                case ( target, Mouse.hasShift modifiers ) of
                    ( OnNode id, False ) ->
                        StartMovingObject id

                    ( OnNode id, True ) ->
                        StartCreatingMorphismFrom id

                    ( OnBackground, _ ) ->
                        NoOp
        , GraphView.onDragBy DragBy
        , GraphView.onDragEnd DragEnd
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { graphView } =
    GraphView.subscriptions GraphViewMsg graphView
