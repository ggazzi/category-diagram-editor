module Demo.View exposing (view)

import Demo.Model exposing (..)
import Demo.Update exposing (..)
import Diagram exposing (Diagram)
import GraphView exposing (Shape(..))
import Html exposing (Html, div, ul, li, text)


view : Model -> Html Msg
view ({ diagram } as model) =
    div []
        [ instructions
        , GraphView.view
            GraphViewMsg
            graphViewConfig
            (objectsAsNodes diagram)
            (morphismsAsEdges diagram ++ morphismBeingCreated model)
        ]


instructions : Html msg
instructions =
    div []
        [ ul []
            [ li [] [ text "Drag objects to move them" ]
            , li [] [ text "Shift+click on the background to add objects" ]
            , li [] [ text "Shift+drag between nodes to add an morphisms" ]
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
