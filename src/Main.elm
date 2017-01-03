module Main exposing (..)

import Diagram exposing (Diagram, Object, ObjectId, Morphism)
import GraphView exposing (Shape(..), Target(..))
import Html exposing (Html, div, ul, li, text)
import Mouse.Modifiers as Mouse
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
      , uid = 0
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
    , uid : Int
    , interaction : InteractionState
    , graphView : GraphView.State
    }


findUniqueId : Model -> ObjectId
findUniqueId { uid, diagram } =
    let
        find candidate =
            if diagram |> Diagram.containsObject candidate then
                find (candidate + 1)
            else
                candidate
    in
        find uid


type InteractionState
    = Idle
    | MovingObject ObjectId



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



-- VIEW


view : Model -> Html Msg
view ({ diagram } as model) =
    div []
        [ instructions
        , GraphView.view
            GraphViewMsg
            (objectsAsNodes diagram)
            (morphismsAsEdges diagram)
        ]


instructions : Html msg
instructions =
    div []
        [ ul []
            [ li [] [ text "Drag objects to move them" ]
            , li [] [ text "Shift+click on the background to add objects" ]
            ]
        ]


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
