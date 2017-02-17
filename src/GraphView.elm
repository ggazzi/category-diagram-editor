module GraphView
    exposing
        ( Node
        , NodeId
        , Edge
        , Endpoint
        , Shape(..)
        , State
        , init
        , UpdateConfig
        , Event
        , Target(..)
        , updateConfig
        , onClick
        , onDragStart
        , onDragEnd
        , onDragBy
        , onMouseUp
        , Msg
        , Output(..)
        , update
        , subscriptions
        , view
        )

import Draggable as Draggable exposing (Delta)
import Draggable.Events as Draggable
import Html exposing (Html)
import Html.Attributes exposing (style)
import Json.Decode as Json
import Mouse.State as Mouse
import Position exposing (Position)
import Svg exposing (Svg)
import Svg.Attributes as Attr
import Svg.Events as Svg
import Svg.Keyed


-- VIEW MODEL


{-| Type containing graphical information about a node.
-}
type alias Node =
    { id : NodeId
    , name : String
    , x : Float
    , y : Float
    , shape : Shape
    , selected : Bool
    }


{-| Type used to identify nodes.
-}
type alias NodeId =
    Int


{-| Type containing graphical information about an edge.
-}
type alias Edge =
    { source : Endpoint
    , target : Endpoint
    }


{-| Type containing graphical information about an edge's endpoint, which may not be an actual node.
-}
type alias Endpoint =
    { key : Maybe Int
    , x : Float
    , y : Float
    , shape : Shape
    }


type Shape
    = None
    | Circle Float


{-| Type representing possible targets of interaction.
-}
type Target
    = OnBackground
    | OnNode NodeId



-- MODEL


{-| Internal state of the graph view.
-}
type alias State =
    { interaction : InteractionState
    , drag : Draggable.State ()
    }


type InteractionState
    = Idle
    | AwaitingInteraction (Mouse.State Target)


{-| Initial state for the graph view.
-}
init : State
init =
    { interaction = Idle
    , drag = Draggable.init
    }



-- CONFIG


{-| Configuration for the @update@ function of the graph view. This describes the messages that will be output by the component, to the parent application, for different events from the view.
-}
type UpdateConfig msg
    = UpdateConfig
        { onClick : Mouse.State Target -> Maybe msg
        , onDragStart : Mouse.State Target -> Maybe msg
        , onDragBy : Delta -> Maybe msg
        , onDragEnd : Maybe msg
        , onMouseUp : Target -> Maybe msg
        }


type Event msg
    = Event (UpdateConfig msg -> UpdateConfig msg)


emptyConfig : UpdateConfig msg
emptyConfig =
    UpdateConfig
        { onClick = \_ -> Nothing
        , onDragStart = \_ -> Nothing
        , onDragBy = \_ -> Nothing
        , onDragEnd = Nothing
        , onMouseUp = \_ -> Nothing
        }


updateConfig : List (Event msg) -> UpdateConfig msg
updateConfig events =
    let
        applyAllTo initial =
            List.foldl (\(Event f) acc -> f acc) initial
    in
        events
            |> applyAllTo emptyConfig


onClick : (Mouse.State Target -> Maybe msg) -> Event msg
onClick envelope =
    Event <| \(UpdateConfig config) -> UpdateConfig { config | onClick = envelope }


onDragStart : (Mouse.State Target -> Maybe msg) -> Event msg
onDragStart envelope =
    Event <| \(UpdateConfig config) -> UpdateConfig { config | onDragStart = envelope }


onDragBy : (Delta -> Maybe msg) -> Event msg
onDragBy envelope =
    Event <| \(UpdateConfig config) -> UpdateConfig { config | onDragBy = envelope }


onDragEnd : Maybe msg -> Event msg
onDragEnd message =
    Event <| \(UpdateConfig config) -> UpdateConfig { config | onDragEnd = message }


onMouseUp : (Target -> Maybe msg) -> Event msg
onMouseUp envelope =
    Event <| \(UpdateConfig config) -> UpdateConfig { config | onMouseUp = envelope }



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


{-| Handle internal update messages for the view model.
-}
update : (Msg -> msg) -> UpdateConfig msg -> Msg -> State -> ( State, Output msg )
update envelope (UpdateConfig config) msg ({ interaction } as model) =
    case msg of
        OnClick ->
            case interaction of
                AwaitingInteraction mouseState ->
                    ( model, asOutput <| config.onClick mouseState )

                Idle ->
                    ( model, NoOutput )

        OnDragStart ->
            case interaction of
                AwaitingInteraction mouseState ->
                    ( model, asOutput <| config.onDragStart mouseState )

                Idle ->
                    ( model, NoOutput )

        OnDragBy delta ->
            ( model, asOutput <| config.onDragBy delta )

        OnDragEnd ->
            ( model, asOutput <| config.onDragEnd )

        OnMouseUp target ->
            ( model, asOutput <| config.onMouseUp target )

        DragMsgWithMouseState dragMsg mouseState ->
            let
                ( newModel, cmd ) =
                    Draggable.update draggableConfig dragMsg model
            in
                ( { newModel | interaction = AwaitingInteraction mouseState }
                , OutCmd <| Cmd.map envelope cmd
                )

        DragMsg dragMsg ->
            let
                ( newModel, cmd ) =
                    Draggable.update draggableConfig dragMsg model
            in
                ( newModel, OutCmd <| Cmd.map envelope cmd )


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



-- SUBSCRIPTIONS


{-| Create mouse subscriptions used for dragging.
-}
subscriptions : (Msg -> msg) -> State -> Sub msg
subscriptions envelope { drag } =
    Draggable.subscriptions (envelope << DragMsg) drag



-- VIEW


{-|
Create a graph view. The first argument wraps internal messagens into an envelope for the outer application. The `Config` argument describes the messages generated by the view events. The `List Node` and `List Edge` arguments provide the graph to be displayed. The `List (Svg msg)` argument provides elements that should be displayed alongside the nodes and edges.

  __Note:__ The `List Node`, `List Edge` and `List (Svg msg)` arguments should be computed with information from the model, but generally not stored in it. The `Config` is view or update code and should generally be kept separate from the model.
-}
view : (Msg -> msg) -> List Node -> List Edge -> List (Svg msg) -> Html msg
view envelope nodes edges children =
    Svg.svg
        [ style
            [ ( "margin", "20px" )
            , ( "width", "800px" )
            , ( "height", "600px" )
            ]
        ]
        ([ Svg.defs [] [ arrowhead.svg ]
         , background envelope
         , edges
            |> List.map (\e -> ( toString <| edgeKey e, edgeView e ))
            |> Svg.Keyed.node "g"
                [ Attr.class "edges-view"
                , Attr.stroke "black"
                , Attr.cursor "pointer"
                ]
         , nodes
            |> List.map (\node -> ( toString node.id, nodeView envelope node ))
            |> Svg.Keyed.node "g" [ Attr.class "nodes-view" ]
         ]
            ++ children
        )


background : (Msg -> msg) -> Svg msg
background envelope =
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
            ++ handlerAttributes envelope OnBackground
        )
        []



-- NODES


nodeView : (Msg -> msg) -> Node -> Svg msg
nodeView envelope { id, name, x, y, shape, selected } =
    let
        nodeWrapper =
            Svg.g
                ([ Attr.class "node"
                 , Attr.transform translate
                 , Attr.cursor "pointer"
                 ]
                    ++ handlerAttributes envelope (OnNode id)
                )

        translate =
            "translate(" ++ toString x ++ "," ++ toString y ++ ")"
    in
        nodeWrapper <|
            case shape of
                Circle radius ->
                    [ Svg.circle
                        [ Attr.r (toString radius)
                        , if selected then
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

                None ->
                    []



-- EDGES


edgeKey : { e | source : { b | key : a }, target : { d | key : c } } -> String
edgeKey { source, target } =
    toString ( source.key, target.key )


edgeView : Edge -> Svg msg
edgeView edge =
    let
        { source, target, length } =
            adjustEndpoints edge
    in
        Svg.path
            [ Attr.class "morphism"
            , Attr.stroke edgeColor
            , Attr.strokeWidth "1.5"
            , Attr.markerEnd ("url(#" ++ arrowhead.id ++ ")")
            , Attr.d <|
                if length == 0 then
                    "M0 0"
                    -- Dummy path, nothing should be shown
                else
                    moveTo source ++ lineTo target
            ]
            []


adjustEndpoints : Edge -> { source : Position, target : Position, length : Float }
adjustEndpoints { source, target } =
    let
        displaceBy amount { x, y } =
            { x = x + amount * dx / length
            , y = y + amount * dy / length
            }

        ( dx, dy ) =
            ( target.x - source.x, target.y - source.y )

        length =
            sqrt (dx * dx + dy * dy)
    in
        { source = source |> displaceBy (sourceDisplacement source)
        , target = target |> displaceBy (targetDisplacement target)
        , length = length
        }


sourceDisplacement : Endpoint -> Float
sourceDisplacement { shape } =
    case shape of
        None ->
            0

        Circle radius ->
            radius


targetDisplacement : Endpoint -> Float
targetDisplacement { shape } =
    case shape of
        None ->
            -arrowhead.length

        Circle radius ->
            -(radius + arrowhead.length)



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
