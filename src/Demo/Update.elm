module Demo.Update
    exposing
        ( Msg(..)
        , update
        , subscriptions
        )

import Demo.Model exposing (..)
import Diagram exposing (Diagram, ObjectId)
import Diagram.Selection as Selection
import GraphView as GraphView exposing (Target(..), Event)
import Mouse.Modifiers as Mouse
import Position exposing (Position, Delta, moveBy)


-- UPDATE


type Msg
    = NoOp
    | SetDiagram Diagram
      -- Selection of objects
    | ClearSelection
    | SelectObject ObjectId SelectionMode
    | StartSelectingRectangle Position SelectionMode
      -- Creation of objects/morphisms
    | CreateObjectAt Position
    | StartCreatingMorphismFrom ObjectId
    | CreateMorphismTo ObjectId
      -- Moving objects
    | StartMovingObjects ObjectId
      -- Drag-related events
    | DragBy Delta
    | DragEnd
    | GraphViewMsg GraphView.Msg


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

        StartSelectingRectangle position mode ->
            ( { model
                | interaction = SelectingRectangle { start = position, end = position, mode = mode }
              }
            , Cmd.none
            )

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

        -- Moving objects
        StartMovingObjects objectId ->
            if selection |> Selection.hasObject objectId then
                ( { model | interaction = MovingObjects <| Selection.objects selection }, Cmd.none )
            else
                ( { model | interaction = MovingObjects [ objectId ] }, Cmd.none )

        -- Drag-related events
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

                SelectingRectangle ({ end } as rectangle) ->
                    ( { model
                        | interaction = SelectingRectangle { rectangle | end = end |> moveBy delta }
                      }
                    , Cmd.none
                    )

                Idle ->
                    ( model, Cmd.none )

        DragEnd ->
            case interaction of
                SelectingRectangle { start, end, mode } ->
                    let
                        selectedObjects =
                            diagram |> getObjectsWithinRectangle start end

                        newSelection =
                            case mode of
                                SetSelection ->
                                    Selection.fromObjectsAndMorphisms selectedObjects []

                                AddToSelection ->
                                    selection |> Selection.addObjects selectedObjects
                    in
                        ( { model
                            | selection = newSelection
                            , interaction = Idle
                          }
                        , Cmd.none
                        )

                _ ->
                    ( { model | interaction = Idle }, Cmd.none )

        GraphViewMsg graphViewMsg ->
            let
                ( newGraphViewModel, event ) =
                    GraphView.update graphViewMsg model.graphView

                newModel =
                    { model | graphView = newGraphViewModel }
            in
                case event |> Maybe.andThen interpretEvent of
                    Just msg ->
                        update msg newModel

                    Nothing ->
                        ( newModel, Cmd.none )


interpretEvent : Event -> Maybe Msg
interpretEvent event =
    case event of
        GraphView.MouseUp target ->
            case target of
                OnBackground ->
                    Nothing

                OnNode id ->
                    Just (CreateMorphismTo id)

        GraphView.Click { target, modifiers, position } ->
            case ( target, Mouse.hasShift modifiers ) of
                ( OnBackground, False ) ->
                    Just ClearSelection

                ( OnBackground, True ) ->
                    Just (CreateObjectAt position)

                ( OnNode id, False ) ->
                    Just (SelectObject id SetSelection)

                ( OnNode id, True ) ->
                    Just (SelectObject id AddToSelection)

        GraphView.DragStart { target, modifiers, position } ->
            case ( target, Mouse.hasShift modifiers ) of
                ( OnNode id, False ) ->
                    Just (StartMovingObjects id)

                ( OnNode id, True ) ->
                    Just (StartCreatingMorphismFrom id)

                ( OnBackground, False ) ->
                    Just (StartSelectingRectangle position SetSelection)

                ( OnBackground, True ) ->
                    Just (StartSelectingRectangle position AddToSelection)

        GraphView.DragBy delta ->
            Just (DragBy delta)

        GraphView.DragEnd ->
            Just DragEnd



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { graphView } =
    GraphView.subscriptions GraphViewMsg graphView
