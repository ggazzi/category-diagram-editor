module Diagram
    exposing
        ( Diagram
        , Object
        , Morphism
        , ObjectId
        , MorphismId
        , fromObjectsAndMorphisms
        , objects
        , morphisms
        , objectsWithIds
        , morphismsWithIds
        , containsObject
        , getObject
        , insertObject
        , modifyObject
        , insertMorphism
        , encode
        , decode
        )

{-|
This module provides a data structure for representing category theory diagrams, including graphical information such as its layout.
-}

import Graph exposing (Graph, Node, Edge)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)
import IntDict


{-|
Type for category theory diagrams. Contains a set of objects and a set of morphisms between them.

Currently, this doesn't support parallel morphisms.
-}
type alias Diagram =
    Graph Object Morphism


{-| Type for objects in the diagram. Contains its position.
-}
type alias Object =
    { x : Float
    , y : Float
    , name : String
    }


{-| Type for morphisms in the diagram.
-}
type alias Morphism =
    { name : String }


{-| Type used to identify objects in a diagram.
-}
type alias ObjectId =
    Int


{-| Type used to identify morphisms in a diagram.
-}
type alias MorphismId =
    ( ObjectId, ObjectId )


{-| Construct a diagram from a list of objects and another of morphisms.
-}
fromObjectsAndMorphisms : List ( ObjectId, Object ) -> List ( MorphismId, Morphism ) -> Diagram
fromObjectsAndMorphisms objects morphisms =
    let
        makeNode ( id, object ) =
            Node id object

        makeEdge ( ( dom, cod ), morphism ) =
            Edge dom cod morphism
    in
        Graph.fromNodesAndEdges
            (objects |> List.map makeNode)
            (morphisms |> List.map makeEdge)


{-| Obtain all objects contained in a diagram.
-}
objects : Diagram -> List Object
objects =
    Graph.nodes >> List.map .label


{-| Obtain all objects contained in a diagram, along with their IDs.
-}
objectsWithIds : Diagram -> List ( ObjectId, Object )
objectsWithIds =
    Graph.nodes >> List.map (\{ id, label } -> ( id, label ))


{-| Obtain all morphisms contained in a diagram.
-}
morphisms : Diagram -> List ( Object, Morphism, Object )
morphisms diagram =
    let
        lookupObjects { from, label, to } =
            case ( Graph.get from diagram, Graph.get to diagram ) of
                ( Just source, Just target ) ->
                    Just ( source.node.label, label, target.node.label )

                ( Nothing, _ ) ->
                    Debug.log
                        ("Malformed graph, contains edge with invalid source node " ++ toString from)
                        Nothing

                ( _, Nothing ) ->
                    Debug.log
                        ("Malformed graph, contains edge with invalid target node " ++ toString from)
                        Nothing
    in
        Graph.edges diagram |> List.filterMap lookupObjects


{-| Obtain all morphisms contained in a diagram, as well as their IDs.
-}
morphismsWithIds : Diagram -> List ( Object, MorphismId, Morphism, Object )
morphismsWithIds diagram =
    let
        lookupObjects { from, label, to } =
            case ( Graph.get from diagram, Graph.get to diagram ) of
                ( Just source, Just target ) ->
                    Just ( source.node.label, ( from, to ), label, target.node.label )

                ( Nothing, _ ) ->
                    Debug.log
                        ("Malformed graph, contains edge with invalid source node " ++ toString from)
                        Nothing

                ( _, Nothing ) ->
                    Debug.log
                        ("Malformed graph, contains edge with invalid target node " ++ toString from)
                        Nothing
    in
        Graph.edges diagram |> List.filterMap lookupObjects


{-| Check if a particular object identifier is contained in a diagram.
-}
containsObject : ObjectId -> Diagram -> Bool
containsObject id =
    Graph.member id


{-| Obtain the object associated to the given identifier, if one exists.
-}
getObject : ObjectId -> Diagram -> Maybe Object
getObject id =
    Graph.get id >> Maybe.map (.node >> .label)


{-| Modify the object associated to the given identifier, if one exists.
-}
modifyObject : ObjectId -> (Object -> Object) -> Diagram -> Diagram
modifyObject id modify =
    let
        modifyContext ({ node } as context) =
            { context
                | node =
                    { node | label = modify node.label }
            }
    in
        Graph.update id (Maybe.map modifyContext)


{-| Insert an object, associating it to the given identifier. If there was already such an object, replaces it.
-}
insertObject : ObjectId -> Object -> Diagram -> Diagram
insertObject id object =
    Graph.insert
        { node = Node id object
        , incoming = IntDict.empty
        , outgoing = IntDict.empty
        }


{-| Inserts a morphism between two objects.
-}
insertMorphism : ( ObjectId, ObjectId ) -> Morphism -> Diagram -> Diagram
insertMorphism ( dom, cod ) morphism =
    let
        addEdge ({ outgoing } as context) =
            { context | outgoing = outgoing |> IntDict.insert cod morphism }
    in
        Graph.update dom (Maybe.map addEdge)


{-| Encode the diagram as a JSON value.
-}
encode : Diagram -> Encode.Value
encode diagram =
    let
        encodeObject ( id, { x, y, name } ) =
            ( toString id
            , Encode.object
                [ ( "x", Encode.float x )
                , ( "y", Encode.float y )
                , ( "name", Encode.string name )
                ]
            )

        encodeMorphism ( _, ( src, tgt ), { name }, _ ) =
            Encode.object
                [ ( "src", Encode.int src )
                , ( "tgt", Encode.int tgt )
                , ( "name", Encode.string name )
                ]
    in
        Encode.object
            [ ( "objects"
              , diagram
                    |> objectsWithIds
                    |> List.map encodeObject
                    |> Encode.object
              )
            , ( "morphisms"
              , diagram
                    |> morphismsWithIds
                    |> List.map encodeMorphism
                    |> Encode.list
              )
            ]


{-| Read a Diagram from its JSON representation.
-}
decode : Decoder Diagram
decode =
    let
        decodeObject =
            Decode.map3 Object
                (Decode.field "x" Decode.float)
                (Decode.field "y" Decode.float)
                (Decode.field "name" Decode.string)

        decodeMorphism =
            Decode.map3 (\src tgt name -> ( ( src, tgt ), { name = name } ))
                (Decode.field "src" Decode.int)
                (Decode.field "tgt" Decode.int)
                (Decode.field "name" Decode.string)

        convertIds =
            List.filterMap convertId

        convertId ( id, obj ) =
            case String.toInt id of
                Ok id_ ->
                    Just ( id_, obj )

                Err _ ->
                    Nothing
    in
        Decode.map2 fromObjectsAndMorphisms
            (Decode.field "objects" (Decode.keyValuePairs decodeObject)
                |> Decode.map convertIds
            )
            (Decode.field "morphisms" <| Decode.list decodeMorphism)
