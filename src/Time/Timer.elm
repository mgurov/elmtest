module Time.Timer exposing (..)

-- Show the current time in your time zone.
--
-- Read how it works:
--   https://guide.elm-lang.org/effects/time.html
--
-- For an analog clock, check out this SVG example:
--   https://elm-lang.org/examples/clock
--

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task
import Time
import Json.Decode exposing (string)



-- MAIN


main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , color: String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) "red"
  , Task.perform AdjustTimeZone Time.here
  )



-- UPDATE


type Msg
  = Tick Time.Posix
  | AdjustTimeZone Time.Zone
  | SetColor String



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )

    AdjustTimeZone newZone ->
      ( { model | zone = newZone }
      , Cmd.none
      )

    SetColor color -> 
      ( { model | color = color }
      , Cmd.none
      )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
  let
    hour   = timePartToString (Time.toHour   model.zone model.time)
    minute = timePartToString (Time.toMinute model.zone model.time)
    second = timePartToString (Time.toSecond model.zone model.time)
  in
  div [] [
    h1 [style "color" model.color] [
      text (hour ++ ":" ++ minute ++ ":" ++ second)
    ]
    , div [] [
      setColorButton "green"
      ,setColorButton "red"
      ,setColorButton "grey"
    ]
  ]

-- view hjelpers
setColorButton: String -> Html Msg
setColorButton color = 
  button [onClick (SetColor color)] [text(color)]

timePartToString part = 
  String.pad 2 '0' (String.fromInt part)
    