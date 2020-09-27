module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (rows)
import Html.Attributes exposing (cols)

import Json.Decode as Jdec
import Json.Decode.Pipeline as Jpipe
import Json.Encode as Jenc
import Dict exposing (Dict, map, toList)
import Array exposing (Array, map)



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
    
    "tonight": "yeah",
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
    , div [] [ 
      textarea [rows 20, cols  40] [text model.json] 
      , pre [] [text (welcome model.json)]
    ]
    
    ]

    -- TODO: json pretty formatting

--  JSON decoding


type alias Welcome =
    { something : String
    , tonight : String
    , inval : Int
    }

-- decoders and encoders

-- welcomeToString : Welcome -> String
-- welcomeToString r = Jenc.encode 0 (encodeWelcome r)

welcome : Jdec.Decoder Welcome
welcome =
    Jdec.succeed Welcome
        |> Jpipe.required "something" Jdec.string
        |> Jpipe.required "tonight" Jdec.string
        |> Jpipe.required "inval" Jdec.int

-- encodeWelcome : Welcome -> Jenc.Value
-- encodeWelcome x =
--     Jenc.object
--         [ ("something", Jenc.string x.something)
--         , ("tonight", Jenc.string x.tonight)
--         , ("inval", Jenc.int x.inval)
--         ]

-- --- encoder helpers

-- makeArrayEncoder : (a -> Jenc.Value) -> Array a -> Jenc.Value
-- makeArrayEncoder f arr =
--     Jenc.array (Array.map f arr)

-- makeDictEncoder : (a -> Jenc.Value) -> Dict String a -> Jenc.Value
-- makeDictEncoder f dict =
--     Jenc.object (toList (Dict.map (\k -> f) dict))

-- makeNullableEncoder : (a -> Jenc.Value) -> Maybe a -> Jenc.Value
-- makeNullableEncoder f m =
--     case m of
--     Just x -> f x
--     Nothing -> Jenc.null
