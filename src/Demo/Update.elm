module Demo.Update
    exposing
        ( Msg(..)
        , update
        , subscriptions
        , graphViewConfig
        )

import Demo.Model exposing (..)
import Diagram exposing (Diagram, ObjectId)
import Diagram.Selection as Selection
import GraphView exposing (Target(..))
import Json.Decode
import Mouse.Modifiers as Mouse
import Position exposing (Position, Delta, moveBy)


-- UPDATE


type Msg
    = NoOp
    | SetDiagram Diagram
      -- Selection of objects
    | ClearSelection
    | SelectObject ObjectId SelectionMode
      -- Creation of objects/morphisms
    | CreateObjectAt Position
    | StartCreatingMorphismFrom ObjectId
    | CreateMorphismTo ObjectId
      -- Drag-related events
    | StartMovingObjects ObjectId
    | DragBy Delta
    | DragEnd
    | GraphViewMsg GraphView.InternalMsg


type SelectionMode
    = SetSelection
    | AddToSelection


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ diagram, interaction, selection } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetDiagram newDiagram ->
            ( { model | diagram = newDiagram }, Cmd.none )

        -- Selection of objects
        ClearSelection ->
            ( { model | selection = Selection.empty }, Cmd.none )

        SelectObject objectId mode ->
            case mode of
                SetSelection ->
                    let
                        theObject =
                            Selection.singleObject objectId
                    in
                        if selection == theObject then
                            ( { model | selection = Selection.empty }, Cmd.none )
                        else
                            ( { model | selection = theObject }, Cmd.none )

                AddToSelection ->
                    if selection |> Selection.hasObject objectId then
                        ( { model | selection = selection |> Selection.removeObject objectId }, Cmd.none )
                    else
                        ( { model | selection = selection |> Selection.addObject objectId }, Cmd.none )

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
                    , selection =
                        Selection.empty
                        -- When a new object is created, the selection should be reset
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
        StartMovingObjects objectId ->
            if selection |> Selection.hasObject objectId then
                ( { model | interaction = MovingObjects <| Selection.objects selection }, Cmd.none )
            else
                ( { model | interaction = MovingObjects [ objectId ] }, Cmd.none )

        DragBy delta ->
            case interaction of
                MovingObjects objectIds ->
                    ( { model
                        | diagram = diagram |> Diagram.modifyObjects objectIds (moveBy delta)
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
                        Just (Json.Decode.succeed (CreateMorphismTo id))
        , GraphView.onClick <|
            \{ target, modifiers, position } ->
                case ( target, Mouse.hasShift modifiers ) of
                    ( OnBackground, False ) ->
                        ClearSelection

                    ( OnBackground, True ) ->
                        CreateObjectAt position

                    ( OnNode id, False ) ->
                        SelectObject id SetSelection

                    ( OnNode id, True ) ->
                        SelectObject id AddToSelection
        , GraphView.onDragStart <|
            \{ target, modifiers } ->
                case ( target, Mouse.hasShift modifiers ) of
                    ( OnNode id, False ) ->
                        StartMovingObjects id

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
