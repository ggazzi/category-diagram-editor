module Demo.View exposing (view)

import Demo.Model exposing (..)
import Demo.Update exposing (..)
import Diagram exposing (Diagram, Object, ObjectId, Morphism, MorphismId)
import Diagram.Selection as Selection exposing (Selection)
import GraphView exposing (Shape(..), Endpoint)
import Html exposing (Html, h1, h2, div, ul, li, p, text, textarea)
import Html.Attributes as Html exposing (style)
import Html.Events as Html
import Json.Encode
import Json.Decode
import Position exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import MeasureText exposing (measureText)


view : Model -> Html Msg
view ({ diagram, selection, interaction } as model) =
    div
        [ style
            [ ( "max-width", "60em" )
            , ( "margin", "0.7em auto" )
            ]
        ]
        [ h1 [] [ text "Categorial Diagram Editor" ]
        , instructions
        , GraphView.view
            GraphViewMsg
            (diagram |> Diagram.objectsWithIds |> List.map (objectToNode selection))
            (morphismBeingCreated model
                ++ List.map morphismToEdge (Diagram.morphismsWithIds diagram)
            )
            [ selectionRectangle interaction ]
        , textView diagram
        ]


instructions : Html msg
instructions =
    let
        section title items =
            [ h2 [] [ text title ]
            , ul [] <| List.map (\x -> li [] [ text x ]) items
            ]
    in
        (div [] << List.concat) <|
            [ section "Selection"
                [ "Click to select a single object"
                , "Shift+click to add an object to the selection"
                , "Drag on the background to select multiple objects within a rectangle"
                , "Shift+drag on the background to add multiple objects to the selection"
                , "Click the background to clear the selection"
                ]
            , section "Moving Objects"
                [ "Drag objects to move them"
                , "Dragging a selected object will move all selected objects"
                ]
            , section "Adding Objects/Morphisms"
                [ "Shift+click the background to add objects"
                , "Shift+drag between objects to add morphisms"
                ]
            ]


textView : Diagram -> Html Msg
textView diagram =
    let
        parseValue val =
            case Json.Decode.decodeString Diagram.decode val of
                Ok newDiagram ->
                    SetDiagram newDiagram

                Err _ ->
                    NoOp
    in
        div []
            [ h2 [] [ text "JSON Serialization" ]
            , p []
                [ text
                    """The following textbox contains a JSON serialization of the diagram shown
                       above. It is kept updated with any changes to the diagram, and the diagram is
                       also updated with any changes to the JSON. Beware of editing directly into
                       the textarea: if at any point the JSON is not well-formed, the behavior will
                       be undefined.
                    """
                ]
            , textarea
                [ Html.value (Json.Encode.encode 2 (Diagram.encode diagram))
                , Html.rows 30
                , Html.cols 90
                , Html.onInput parseValue
                ]
                []
            ]


morphismBeingCreated : Model -> List GraphView.Edge
morphismBeingCreated { interaction, diagram } =
    case interaction of
        CreatingMorphismFrom domainId mousePos ->
            case Diagram.getObject domainId diagram of
                Just domain ->
                    [ { id = ( domainId, -1 )
                      , source = objectToEndpoint domain
                      , target =
                            { x = mousePos.x
                            , y = mousePos.y
                            , shape = NoShape
                            }
                      }
                    ]

                Nothing ->
                    []

        _ ->
            []



-- GRAPH VIEW CONFIG


type ViewEdge
    = ActualMorphism ( Object, ( ObjectId, ObjectId ), Morphism, Object )
    | VirtualMorphism ObjectId Position String


objectToNode : Selection -> ( ObjectId, Object ) -> GraphView.Node msg
objectToNode selection ( id, { name, x, y } ) =
    { id = id
    , x = x
    , y = y
    , view =
        let
            width =
                nodeSize name + 6
        in
            [ Svg.rect
                [ Attr.width (toString width)
                , Attr.height "25"
                , Attr.x (toString <| -width / 2)
                , Attr.y "-12.5"
                , Attr.rx "3"
                , Attr.ry "3"
                , if selection |> Selection.hasObject id then
                    Attr.fill "lightgrey"
                  else
                    Attr.fill "white"
                , Attr.stroke "lightgrey"
                , Attr.strokeWidth "1"
                ]
                []
            , Svg.text_
                [ Attr.textAnchor "middle"
                , Attr.y "5"
                ]
                [ Svg.text name
                ]
            ]
    }


morphismToEdge : ( Object, MorphismId, Morphism, Object ) -> GraphView.Edge
morphismToEdge ( domain, id, _, codomain ) =
    { id = id
    , source = objectToEndpoint domain
    , target = objectToEndpoint codomain
    }


objectToEndpoint : Object -> Endpoint
objectToEndpoint { x, y, name } =
    { x = x, y = y, shape = Rectangle (nodeSize name + 6) 25 }


nodeSize : String -> Float
nodeSize name =
    toFloat <| max 19 (measureText "16px sans-serif" name)


nodeShape : Shape
nodeShape =
    Rectangle 30 30


selectionRectangle : InteractionState -> Svg msg
selectionRectangle interaction =
    case interaction of
        SelectingRectangle { start, end } ->
            Svg.path
                [ Attr.fill "transparent"
                , Attr.stroke "lightgrey"
                , Attr.strokeWidth "1"
                , Attr.d <|
                    moveTo start
                        ++ lineTo { x = start.x, y = end.y }
                        ++ lineTo end
                        ++ lineTo { x = end.x, y = start.y }
                        ++ closePath
                ]
                []

        _ ->
            Svg.g [] []



-- UTILITIES


type alias PathFragment =
    String


moveTo : Position -> PathFragment
moveTo { x, y } =
    "M" ++ toString x ++ " " ++ toString y


lineTo : Position -> PathFragment
lineTo { x, y } =
    "L" ++ toString x ++ " " ++ toString y


closePath : PathFragment
closePath =
    "Z"
