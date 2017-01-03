module Main exposing (..)

import Diagram exposing (Diagram, Object, Morphism)
import Draggable
import Draggable.Events as Draggable
import Diagram exposing (Diagram, Object, ObjectId, Morphism)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Position exposing (Position, Delta, moveBy)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Keyed


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
      , drag = Draggable.init
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
    , drag : Draggable.State
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
    | DragMsg Draggable.Msg


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

        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model


dragConfig : Draggable.Config Msg
dragConfig =
    let
        startDragging key =
            case String.toInt key of
                Err _ ->
                    NoOp

                Ok id ->
                    StartMovingObject id
    in
        Draggable.customConfig
            [ Draggable.onDragStart startDragging
            , Draggable.onDragBy DragBy
            , Draggable.onDragEnd DragEnd
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag



-- VIEW


view : Model -> Html Msg
view model =
    Svg.svg
        [ style
            [ ( "margin", "20px" )
            , ( "width", "800px" )
            , ( "height", "600px" )
            ]
        ]
        [ Svg.defs [] [ arrowhead.svg ]
        , background
        , morphismsView model.diagram
        , objectsView model.diagram
        ]


type alias Marker a =
    { id : String
    , length : Float
    , svg : Svg a
    }


edgeColor : String
edgeColor =
    "black"


arrowhead : Marker a
arrowhead =
    { id = "arrowhead"
    , length = 2
    , svg =
        Svg.marker
            [ Attr.id "arrowhead"
            , Attr.viewBox "0 -5 10 10"
            , Attr.refX "8"
            , Attr.markerWidth "4"
            , Attr.markerHeight "6"
            , Attr.orient "auto"
            ]
            [ Svg.path
                [ Attr.d "M0,-5L10,0L0,5"
                , Attr.fill "transparent"
                , Attr.stroke edgeColor
                , Attr.strokeWidth "2"
                ]
                []
            ]
    }


background : Svg Msg
background =
    Svg.rect
        [ Attr.width "100%"
        , Attr.height "100%"
        , Attr.fill "transparent"
        , Attr.stroke "lightgrey"
        , Attr.strokeWidth "2px"
        , Attr.rx "5px"
        , Attr.ry "5px"
        ]
        []


morphismsView : Diagram -> Svg Msg
morphismsView diagram =
    let
        keyedMorphismView ( src, id, edge, tgt ) =
            ( toString id, morphismView src edge tgt )
    in
        diagram
            |> Diagram.morphismsWithIds
            |> List.map keyedMorphismView
            |> Svg.Keyed.node "g"
                [ Attr.class "morphisms-view"
                , Attr.stroke "black"
                ]


morphismView : Object -> Morphism -> Object -> Svg Msg
morphismView src edge tgt =
    let
        -- Displace endpoints according to object radius
        src_ =
            displaceBy objectRadius src

        tgt_ =
            displaceBy -(objectRadius + arrowhead.length) tgt

        displaceBy amount { x, y } =
            { x = x + amount * dx / length
            , y = y + amount * dy / length
            }

        dx =
            tgt.x - src.x

        dy =
            tgt.y - src.y

        length =
            sqrt (dx * dx + dy * dy)

        path =
            ("M" ++ toString src_.x ++ "," ++ toString src_.y)
                ++ ("L" ++ toString tgt_.x ++ "," ++ toString tgt_.y)
    in
        Svg.path
            [ Attr.class "morphism"
            , Attr.d path
            , Attr.stroke edgeColor
            , Attr.strokeWidth "2"
            , Attr.markerEnd ("url(#" ++ arrowhead.id ++ ")")
            ]
            []


objectRadius : Float
objectRadius =
    20


objectsView : Diagram -> Svg Msg
objectsView diagram =
    diagram
        |> Diagram.objectsWithIds
        |> List.map (\( id, obj ) -> ( toString id, objectView id obj ))
        |> Svg.Keyed.node "g"
            [ Attr.class "objects-view"
            ]


objectView : ObjectId -> Object -> Svg Msg
objectView id { x, y, name } =
    let
        translate =
            "translate("
                ++ toString x
                ++ ","
                ++ toString y
                ++ ")"
    in
        Svg.g
            [ Attr.class "object"
            , Attr.transform translate
            , Draggable.mouseTrigger (toString id) DragMsg
            ]
            [ Svg.circle
                [ Attr.r (toString objectRadius)
                , Attr.fill "transparent"
                , Attr.stroke "lightgrey"
                , Attr.strokeWidth "1px"
                ]
                []
            , Svg.text_ [ Attr.textAnchor "middle", Attr.y "5" ]
                [ Svg.text name
                ]
            ]
