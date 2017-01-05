module Demo.View exposing (view)

import Demo.Model exposing (..)
import Demo.Update exposing (..)
import Diagram exposing (Diagram)
import Diagram.Selection as Selection exposing (Selection)
import GraphView exposing (Shape(..))
import Html exposing (Html, h1, h2, div, ul, li, p, text, textarea)
import Html.Attributes as Html exposing (style)
import Html.Events as Html
import Json.Encode
import Json.Decode
import Position exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as Attr


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
            graphViewConfig
            (objectsAsNodes diagram selection)
            (morphismsAsEdges diagram ++ morphismBeingCreated model)
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


objectsAsNodes : Diagram -> Selection -> List GraphView.Node
objectsAsNodes diagram selection =
    diagram
        |> Diagram.objectsWithIds
        |> List.map
            (\( id, { x, y, name } ) ->
                { id = id
                , name = name
                , x = x
                , y = y
                , shape = nodeShape
                , selected = selection |> Selection.hasObject id
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


morphismBeingCreated : Model -> List GraphView.Edge
morphismBeingCreated { interaction, diagram } =
    case interaction of
        CreatingMorphismFrom domainId mousePos ->
            case diagram |> Diagram.getObject domainId of
                Just domain ->
                    [ { source = { x = domain.x, y = domain.y, key = Just domainId, shape = nodeShape }
                      , target = { x = mousePos.x, y = mousePos.y, key = Nothing, shape = None }
                      }
                    ]

                Nothing ->
                    []

        _ ->
            []


nodeShape : Shape
nodeShape =
    Circle 15



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
