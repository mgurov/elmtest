module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, span, text, textarea)
import Html.Events exposing (onClick)
import Html.Attributes exposing (rows)
import Html.Attributes exposing (cols)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
  { count: Int
  , json: String}


init : Model
init =
  Model 0 """
  {
    "something": "in the air",
    
    "tonight": "yeah,
    "inval": 1
  }
  """

-- UPDATE


type Msg
  = Increment
  | Decrement


update : Msg -> Model -> Model
update msg model =
  case msg of
    Increment ->
      {model | count = model.count + 1}

    Decrement ->
      {model | count = model.count - 1}



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Decrement ] [ text "-" ]
    , span [] [ text (String.fromInt model.count) ]
    , button [ onClick Increment ] [ text "+" ]
    , div [] [ textarea [rows 20, cols  80] [text model.json] ]
    
    ]

    -- TODO: json pretty formatting