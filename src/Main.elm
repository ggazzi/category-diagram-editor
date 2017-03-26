module Main exposing (..)

import Main.Model exposing (..)
import Main.Update exposing (..)
import Main.View exposing (..)
import Diagram exposing (Diagram, Object, Morphism)
import Diagram.Selection as Selection
import GraphView exposing (Shape(..), Target(..))
import Html exposing (Html, div, ul, li, text)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { diagram = singleMorphism
      , uid = 100
      , selection = Selection.empty
      , interaction = Idle
      , graphView = GraphView.init
      }
    , Cmd.none
    )


singleMorphism : Diagram
singleMorphism =
    let
        objects =
            [ ( 0, Object 100 100 "A" )
            , ( 1, Object 100 200 "B" )
            ]

        morphisms =
            [ ( ( 0, 1 ), Morphism "f" ) ]
    in
        Diagram.fromObjectsAndMorphisms objects morphisms
