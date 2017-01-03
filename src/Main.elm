module Main exposing (..)

import Diagram exposing (Diagram, Object, Morphism)
import Graph exposing (Graph, Node, Edge)
import Html exposing (Html)
import Html.Attributes exposing (style)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Keyed


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { diagram : Diagram }


model : Model
model =
    { diagram = singleMorphism }


singleMorphism : Diagram
singleMorphism =
    let
        nodes =
            [ Node 0 (Object 100 100 "A")
            , Node 1 (Object 100 200 "B")
            ]

        edges =
            [ Edge 0 1 (Morphism "f") ]
    in
        Graph.fromNodesAndEdges nodes edges



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



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
        |> Graph.nodes
        |> List.map (\n -> ( toString n.id, objectView n.label ))
        |> Svg.Keyed.node "g"
            [ Attr.class "objects-view"
            ]


objectView : Object -> Svg Msg
objectView { x, y, name } =
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
