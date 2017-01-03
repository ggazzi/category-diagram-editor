module Mouse.State
    exposing
        ( State
        , state
        )

import Json.Decode as Json
import Position exposing (Position)
import Mouse.Modifiers exposing (Modifiers, modifiers)


{-| Represents the state of the mouse at a particular moment, including its current target, its current position and the modifier keys that were pressed.
-}
type alias State target =
    { target : target
    , position : Position
    , modifiers : Modifiers
    }


{-| JSON decoder that obtains a mouse state from a MouseEvent, given the current target.
-}
state : target -> Json.Decoder (State target)
state target =
    Json.map2 (State target)
        position
        modifiers


position : Json.Decoder Position
position =
    Json.map2 Position
        (Json.field "offsetX" Json.float)
        (Json.field "offsetY" Json.float)
