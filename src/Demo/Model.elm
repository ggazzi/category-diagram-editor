module Demo.Model exposing (Model, InteractionState(..), findUniqueId)

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
