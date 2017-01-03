module Main exposing (..)

import Html exposing (Html)


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    {}


model : Model
model =
    {}



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    Html.text "Placeholder"
