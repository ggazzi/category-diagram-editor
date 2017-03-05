module GraphView
    exposing
        ( State
        , Msg
        , init
        , update
        , subscriptions
        , view
          -- Configuration of Events
        , UpdateConfig
        , Target(..)
        , updateConfig
          -- Configuration of the View
        , ViewConfig
        , viewConfig
        , NodeInfo
        , EdgeInfo
        , Endpoint(..)
        , Shape(..)
        , Output(..)
        )

{-|
This module provides utilities for displaying graphs.

@docs State, Msg, init, update, subscriptions, view

# Configuration of Events
@docs UpdateConfig, Target, updateConfig

# Configuration of the View
@docs ViewConfig, viewConfig, NodeInfo, EdgeInfo, Endpoint, Shape, Output
-}

import Dict exposing (Dict)
import Draggable as Draggable exposing (Delta)
import Draggable.Events as Draggable
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as Json
import Mouse.State as Mouse
import Position exposing (Position, Positioned, positionOf, moveBy)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events as Svg
import Svg.Keyed


-- MODEL


{-| Opaque type representing the internal state of the graph view.
-}
type State
    = State
        { interaction : InteractionState
        , drag : Draggable.State ()
        }


{-|
Temporarily stores the mouse state when the user has pressed a mouse button, but neither a
click nor a drag were identified yet.
-}
type InteractionState
    = Idle
    | AwaitingInteraction (Mouse.State Target)


{-| Initial state for the graph view.
-}
init : State
init =
    State
        { interaction = Idle
        , drag = Draggable.init
        }



-- UPDATE


{-| A message type to update the graph view.
-}
type Msg
    = OnClick
    | OnDragStart
    | OnDragBy Delta
    | OnDragEnd
    | OnMouseUp Target
    | DragMsgWithMouseState (Draggable.Msg ()) (Mouse.State Target)
    | DragMsg (Draggable.Msg ())


{-|
 Possible outputs of updating the graph view: either a message to the parent module,
 a command to be run, or nothing.
-}
type Output msg
    = OutCmd (Cmd msg)
    | OutMsg msg
    | NoOutput


{-|
Handle internal update messages for the view model, returning the updated state and one of the
following:
  * A command that should be sent to the environment
  * A message that should be handled by the containing module
  * No output
-}
update : (Msg -> msg) -> UpdateConfig msg -> Msg -> State -> ( State, Output msg )
update envelope (UpdateConfig config) msg (State ({ interaction } as model)) =
    case msg of
        OnClick ->
            case interaction of
                AwaitingInteraction mouseState ->
                    ( State model, asOutput <| config.onClick mouseState )

                Idle ->
                    ( State model, NoOutput )

        OnDragStart ->
            case interaction of
                AwaitingInteraction mouseState ->
                    ( State model, asOutput <| config.onDragStart mouseState )

                Idle ->
                    ( State model, NoOutput )

        OnDragBy delta ->
            ( State model, asOutput <| config.onDragBy delta )

        OnDragEnd ->
            ( State model, asOutput <| config.onDragEnd )

        OnMouseUp target ->
            ( State model, asOutput <| config.onMouseUp target )

        DragMsgWithMouseState dragMsg mouseState ->
            let
                ( newModel, cmd ) =
                    Draggable.update draggableConfig dragMsg model
            in
                ( State { newModel | interaction = AwaitingInteraction mouseState }
                , OutCmd <| Cmd.map envelope cmd
                )

        DragMsg dragMsg ->
            let
                ( newModel, cmd ) =
                    Draggable.update draggableConfig dragMsg model
            in
                ( State newModel, OutCmd <| Cmd.map envelope cmd )


asOutput : Maybe msg -> Output msg
asOutput maybeMsg =
    case maybeMsg of
        Just msg ->
            OutMsg msg

        Nothing ->
            NoOutput


draggableConfig : Draggable.Config () Msg
draggableConfig =
    Draggable.customConfig
        [ Draggable.onClick (\_ -> OnClick)
        , Draggable.onDragStart (\_ -> OnDragStart)
        , Draggable.onDragBy OnDragBy
        , Draggable.onDragEnd OnDragEnd
        ]



-- UPDATE CONFIG


{-|
Configuration for the `update` function of the graph view. This describes the messages that will be
output by the component, to the parent application, for different events from the view.
-}
type UpdateConfig msg
    = UpdateConfig
        { onClick : Mouse.State Target -> Maybe msg
        , onDragStart : Mouse.State Target -> Maybe msg
        , onDragBy : Delta -> Maybe msg
        , onDragEnd : Maybe msg
        , onMouseUp : Target -> Maybe msg
        }


{-| Type representing possible targets of interaction.
-}
type Target
    = OnBackground
    | OnNode Int


{-| Create a configuration for the update function, handling the given events.
-}
updateConfig :
    { onClick : Mouse.State Target -> Maybe msg
    , onDragStart : Mouse.State Target -> Maybe msg
    , onDragBy : Delta -> Maybe msg
    , onDragEnd : Maybe msg
    , onMouseUp : Target -> Maybe msg
    }
    -> UpdateConfig msg
updateConfig =
    UpdateConfig



-- SUBSCRIPTIONS


{-| Create mouse subscriptions used for dragging.
-}
subscriptions : (Msg -> msg) -> State -> Sub msg
subscriptions envelope (State { drag }) =
    Draggable.subscriptions (envelope << DragMsg) drag



-- VIEW CONFIG


{-| Type for configuring how nodes and edges are identified and displayed.
-}
type ViewConfig msg node edge
    = ViewConfig
        { asMsg : Msg -> msg
        , getNodeInfo : node -> NodeInfo
        , nodeView : node -> List (Svg msg)
        , getEdgeInfo : edge -> EdgeInfo
        }


{-| Configure how nodes and edges should be identified and displayed.
-}
viewConfig :
    { asMsg : Msg -> msg
    , getNodeInfo : node -> NodeInfo
    , nodeView : node -> List (Svg msg)
    , getEdgeInfo : edge -> EdgeInfo
    }
    -> ViewConfig msg node edge
viewConfig =
    ViewConfig


{-| Description of how a node should be displayed.
-}
type alias NodeInfo =
    { id : Int
    , x : Float
    , y : Float
    , shape : Shape
    }


{-| Description of how an edge should be displayed.
-}
type alias EdgeInfo =
    { id : Int
    , source : Endpoint
    , target : Endpoint
    }


{-| Description of edge endpoints, which influence how an edge is displayed.
-}
type Endpoint
    = EndpointNode Int
    | EndpointPosition Position


type alias ResolvedEndpoint =
    { x : Float, y : Float, shape : Shape }


{-| Possible shapes for nodes.
-}
type Shape
    = NoShape
    | Circle Float
    | Rectangle Float Float



-- VIEW


{-|
Create a graph view. The `ViewConfig` argument describes how the nodes and edges will be identified and displayed. The `List node` and `List edge` arguments provide the graph to be displayed. The `List (Svg msg)` argument provides additional elements that should be displayed alongside the nodes and edges.

  __Note:__ The `List Node`, `List Edge` and `List (Svg msg)` arguments should be computed with information from the model, but generally not stored in it. The `ViewConfig` is view code and should _always_ be kept separate from the model.
-}
view : ViewConfig msg node edge -> List node -> List edge -> List (Svg msg) -> Html msg
view config nodes edges additionalElements =
    let
        ( nodeInfos, edgeInfos ) =
            preprocessNodesAndEdges config nodes edges
    in
        Svg.svg
            [ style
                [ ( "margin", "20px" )
                , ( "width", "800px" )
                , ( "height", "600px" )
                ]
            ]
            ([ Svg.defs [] [ arrowhead.svg ]
             , background config
             , Svg.Keyed.node "g"
                [ Attr.class "edges-view"
                , Attr.stroke "black"
                , Attr.cursor "pointer"
                ]
                (List.map
                    (\( key, edge ) -> ( toString key, edgeView edge ))
                    (Dict.toList edgeInfos)
                )
             , Svg.Keyed.node "g"
                [ Attr.class "nodes-view" ]
                (List.map
                    (\( key, node ) -> ( toString key, nodeView config node ))
                    (Dict.toList nodeInfos)
                )
             ]
                ++ additionalElements
            )


preprocessNodesAndEdges : ViewConfig msg node edge -> List node -> List edge -> ( Dict Int ( NodeInfo, node ), Dict Int ( EdgeInfo, ResolvedEndpoint, ResolvedEndpoint ) )
preprocessNodesAndEdges (ViewConfig { getNodeInfo, getEdgeInfo }) nodes edges =
    let
        preprocessNode node =
            let
                nodeInfo =
                    getNodeInfo node
            in
                ( nodeInfo.id, ( nodeInfo, node ) )

        nodeInfos =
            Dict.fromList (List.map preprocessNode nodes)

        preprocessEdge edge =
            let
                edgeInfo =
                    getEdgeInfo edge
            in
                case ( resolveEndpoint nodeInfos edgeInfo.source, resolveEndpoint nodeInfos edgeInfo.target ) of
                    ( Just source_, Just target_ ) ->
                        Just ( edgeInfo.id, ( edgeInfo, source_, target_ ) )

                    _ ->
                        Nothing

        edgeInfos =
            Dict.fromList (List.filterMap preprocessEdge edges)
    in
        ( nodeInfos, edgeInfos )


resolveEndpoint : Dict Int ( NodeInfo, node ) -> Endpoint -> Maybe ResolvedEndpoint
resolveEndpoint nodes endpoint =
    case endpoint of
        EndpointNode key ->
            Dict.get key nodes
                |> Maybe.map (\( { x, y, shape }, _ ) -> { x = x, y = y, shape = shape })

        EndpointPosition { x, y } ->
            Just { x = x, y = y, shape = NoShape }


background : ViewConfig msg node edge -> Svg msg
background (ViewConfig { asMsg }) =
    Svg.rect
        ([ Attr.width "100%"
         , Attr.height "100%"
         , Attr.fill "transparent"
         , Attr.stroke "lightgrey"
         , Attr.strokeWidth "2px"
         , Attr.rx "5px"
         , Attr.ry "5px"
         , Attr.cursor "crosshair"
         ]
            ++ handlerAttributes asMsg OnBackground
        )
        []



-- NODES


nodeView : ViewConfig msg node edge -> ( NodeInfo, node ) -> Svg msg
nodeView (ViewConfig { nodeView, asMsg }) ( { id, x, y, shape }, node ) =
    let
        translate =
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"
    in
        Svg.g
            ([ Attr.class "node"
             , Attr.transform translate
             , Attr.cursor "pointer"
             ]
                ++ handlerAttributes asMsg (OnNode id)
            )
            (nodeView node)



-- EDGES


edgeView : ( EdgeInfo, ResolvedEndpoint, ResolvedEndpoint ) -> Svg msg
edgeView ( _, source, target ) =
    let
        ( adjustedSource, adjustedTarget, length ) =
            adjustEndpoints source target
    in
        Svg.path
            [ Attr.class "edge"
            , Attr.stroke edgeColor
            , Attr.strokeWidth "1.5"
            , Attr.markerEnd ("url(#" ++ arrowhead.id ++ ")")
            , Attr.d <|
                if length == 0 then
                    "M0 0"
                    -- Dummy path, nothing should be shown
                else
                    moveTo adjustedSource ++ lineTo adjustedTarget
            ]
            []


adjustEndpoints : ResolvedEndpoint -> ResolvedEndpoint -> ( Position, Position, Float )
adjustEndpoints source target =
    let
        ( dx, dy ) =
            ( target.x - source.x, target.y - source.y )

        segment =
            { start = source
            , end = target
            , dx = dx
            , dy = dy
            , length = sqrt (dx * dx + dy * dy)
            }

        minimumArrowLength =
            1

        adjusted =
            segment
                |> shrinkSegmentTo
                    ( intersectAtStart segment source.shape
                    , intersectAtEnd segment target.shape - arrowhead.length / segment.length
                    )
    in
        ( positionOf adjusted.start
        , positionOf adjusted.end
        , adjusted.length
        )


type alias Segment a =
    { start : Positioned a
    , end : Positioned a
    , dx : Float
    , dy : Float
    , length : Float
    }


reverseSegment : Segment a -> Segment a
reverseSegment { start, end, dx, dy, length } =
    { start = end, end = start, dx = -dx, dy = -dy, length = length }


shrinkSegmentTo : ( Float, Float ) -> Segment a -> Segment a
shrinkSegmentTo ( tStart, tEnd ) ({ start, end, dx, dy, length } as segment) =
    let
        ( tStart_, tEnd_ ) =
            ( clamp 0 1 tStart, clamp 0 1 tEnd )

        dt =
            tEnd_ - tStart_
    in
        { segment
            | start = { start | x = start.x + tStart_ * dx, y = start.y + tStart_ * dy }
            , end = { end | x = start.x + tEnd_ * dx, y = start.y + tEnd_ * dy }
            , dx = dx * dt
            , dy = dy * dt
            , length = length * dt
        }


{-| Given a line segment and a shape centered at the segment's
starting point, calculate the intersection between them.

The intersection is returned as a parameter `t` for the line segment's
equation. The point of intersection may be calculated with
`(segment.start.x + t * segment.dx, segment.start.y + t * segment.dy)`.

The existence of a unique endpoint is guaranteed since all `Shape`s
are convex, the segment's starting point is within the shape.
-}
intersectAtStart : Segment a -> Shape -> Float
intersectAtStart segment shape =
    let
        t =
            intersectAtEnd (reverseSegment segment) shape
    in
        1 - t


{-| Given a line segment and a shape centered at the segment's
endpoint, calculate the intersection between them.

The intersection is returned as a parameter `t` for the line segment's
equation. The point of intersection may be calculated with
`(segment.start.x + t * segment.dx, segment.start.y + t * segment.dy)`.

The existence of a unique endpoint is guaranteed since all `Shape`s
are convex, the segment's endpoint is within the shape.
-}
intersectAtEnd : Segment a -> Shape -> Float
intersectAtEnd { start, end, dx, dy, length } shape =
    case shape of
        NoShape ->
            1

        Circle radius ->
            1 - (radius / length)

        Rectangle width height ->
            let
                intersect1D length start end delta =
                    let
                        halfLength =
                            length / 2

                        ( min, max ) =
                            ( end - halfLength, end + halfLength )
                    in
                        if delta > 1.0e-5 then
                            (min - start) / delta
                        else if delta < -1.0e-5 then
                            (max - start) / delta
                        else
                            1
            in
                max (intersect1D width start.x end.x dx) (intersect1D height start.y end.y dy)



-- UTILITIES


handlerAttributes : (Msg -> msg) -> Target -> List (Svg.Attribute msg)
handlerAttributes envelope target =
    [ Draggable.customMouseTrigger (Mouse.state target)
        (\dragMsg mouseState -> envelope <| DragMsgWithMouseState dragMsg mouseState)
    , Svg.on "mouseup" (Json.succeed <| envelope <| OnMouseUp target)
    ]


type alias Marker a =
    { id : String
    , length : Float
    , svg : Svg a
    }


type alias PathFragment =
    String


moveTo : Position -> PathFragment
moveTo { x, y } =
    "M" ++ toString x ++ " " ++ toString y


lineTo : Position -> PathFragment
lineTo { x, y } =
    "L" ++ toString x ++ " " ++ toString y



-- VIEW CONSTANTS


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
