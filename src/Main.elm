module Main exposing (..)

import Diagram exposing (Diagram, Object, ObjectId, Morphism)
import GraphView exposing (Shape(..))
import Html exposing (Html)
import Position exposing (Position, Delta, moveBy)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { diagram = singleMorphism
      , interaction = Idle
      , graphView = GraphView.init
      }
    , Cmd.none
    )


singleMorphism : Diagram
singleMorphism =
    let
        objects =
            [ ( 0, Object 100 100 "A" )
            , ( 1, Object 100 200 "B" )
            ]

        morphisms =
            [ ( ( 0, 1 ), Morphism "f" ) ]
    in
        Diagram.fromObjectsAndMorphisms objects morphisms



-- MODEL


type alias Model =
    { diagram : Diagram
    , interaction : InteractionState
    , graphView : GraphView.State
    }


type InteractionState
    = Idle
    | MovingObject ObjectId



-- UPDATE


type Msg
    = NoOp
    | StartMovingObject ObjectId
    | DragBy Delta
    | DragEnd
    | GraphViewMsg GraphView.InternalMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ diagram, interaction } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
        [ GraphView.onDragStart StartMovingObject
        , GraphView.onDragBy DragBy
        , GraphView.onDragEnd DragEnd
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { graphView } =
    GraphView.subscriptions GraphViewMsg graphView



-- VIEW


view : Model -> Html Msg
view ({ diagram } as model) =
    GraphView.view
        GraphViewMsg
        (objectsAsNodes diagram)
        (morphismsAsEdges diagram)


objectsAsNodes : Diagram -> List GraphView.Node
objectsAsNodes =
    Diagram.objectsWithIds
        >> List.map
            (\( id, { x, y, name } ) ->
                { id = id
                , name = name
                , x = x
                , y = y
                , shape = nodeShape
                }
            )


morphismsAsEdges : Diagram -> List GraphView.Edge
morphismsAsEdges =
    Diagram.morphismsWithIds
        >> List.map
            (\( dom, ( domId, codId ), _, cod ) ->
                { source = { x = dom.x, y = dom.y, key = Just domId, shape = nodeShape }
                , target = { x = cod.x, y = cod.y, key = Just codId, shape = nodeShape }
                }
            )


nodeShape : Shape
nodeShape =
    Circle 15
