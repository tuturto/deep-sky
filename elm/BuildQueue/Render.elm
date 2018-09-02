module Render exposing (view)

import Html exposing (..)
import Types exposing (..)

view : Model -> Html Msg
view model =
  div []
  [ text model.message ]
