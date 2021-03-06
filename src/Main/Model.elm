module Main.Model
    exposing
        ( Model
        , InteractionState(..)
        , SelectionMode(..)
        , findUniqueId
        , getObjectsWithinRectangle
        )

import Diagram exposing (Diagram, ObjectId)
import Diagram.Selection exposing (Selection)
import GraphView
import Position exposing (Position)


type alias Model =
    { diagram : Diagram
    , uid : Int
    , selection : Selection
    , interaction : InteractionState
    , graphView : GraphView.State
    }


type InteractionState
    = Idle
    | MovingObjects (List ObjectId)
    | CreatingMorphismFrom ObjectId Position
    | SelectingRectangle { start : Position, end : Position, mode : SelectionMode }


type SelectionMode
    = SetSelection
    | AddToSelection


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


getObjectsWithinRectangle : Position -> Position -> Diagram -> List ObjectId
getObjectsWithinRectangle start end =
    let
        ( left, right ) =
            if start.x < end.x then
                ( start.x, end.x )
            else
                ( end.x, start.x )

        ( top, bottom ) =
            if start.y < end.y then
                ( start.y, end.y )
            else
                ( end.y, start.y )

        isWithinRectangle ( id, { x, y } ) =
            if left <= x && x <= right && top <= y && y <= bottom then
                Just id
            else
                Nothing
    in
        Diagram.objectsWithIds >> List.filterMap isWithinRectangle
