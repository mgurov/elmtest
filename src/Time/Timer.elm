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
import Html.Events exposing (onClick,onInput)
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

type Color = Red | Green | Grey

colorString color = 
  case color of 
    Red -> "red"
    Green -> "green"
    Grey -> "grey"

type alias Model =
  { zone : Time.Zone
  , time : Time.Posix
  , color: Color
  , timerStatus: TimerStatus
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( Model Time.utc (Time.millisToPosix 0) Grey None
  , Task.perform AdjustTimeZone Time.here
  )

type TimerStatus 
                = None 
                | Adding MaybeIntInput
                | Going Int --remaining seconds

type alias MaybeIntInput = 
  { input: String
  , maybeInt: Maybe Int
  }

-- UPDATE


type Msg
  = Tick Time.Posix
  | TimerTick Time.Posix
  | AdjustTimeZone Time.Zone
  | SetColor Color
  | ShowAddTimer String -- input
  | StartTimerMin (Maybe Int) -- duration
  | ResetTimer

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      ( { model | time = newTime }
      , Cmd.none
      )
    
    TimerTick _ -> case model.timerStatus of 
      Going remainder -> 
        let
           deduction = if remainder > 0 then 1 else 0
        in
          (
          {model | timerStatus = Going(remainder - deduction)}
          , Cmd.none
          )
      _ -> (model, Cmd.none)

    ResetTimer -> ( { model | timerStatus = None }
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

    ShowAddTimer input -> ({model | timerStatus = Adding (MaybeIntInput input (String.toInt input))}, Cmd.none)

    StartTimerMin minutesMaybe ->
      case minutesMaybe of
        Nothing -> (model, Cmd.none)
        Just minutes -> ({model | timerStatus = Going (minutes * 60)}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ = Sub.batch [
    Time.every 1000 Tick
    , Time.every 1000 TimerTick
  ]



-- VIEW


view : Model -> Html Msg
view model =
  div [] [
    case model.timerStatus of 
      Going remainder -> 
        let
            seconds = modBy 60 remainder
            minutes = remainder // 60
        in
        
        h4 [style "color" (colorString model.color)] [
          text (timePartToString minutes ++ ":" ++ timePartToString seconds)
        ]
      _ -> 
        let
          hour   = timePartToString (Time.toHour   model.zone model.time)
          minute = timePartToString (Time.toMinute model.zone model.time)
          second = timePartToString (Time.toSecond model.zone model.time)
        in
          h4 [style "color" (colorString model.color)] [
            text (hour ++ ":" ++ minute ++ ":" ++ second)
          ]
    , hr [] []
    ,div [] [
      timerControlView model.timerStatus
    ]
    , div [] [
      text "time color: "
      ,setColorButton Green
      ,setColorButton Red
      ,setColorButton Grey
    ]
  ]

timerControlView: TimerStatus -> Html Msg
timerControlView timerStatus = 
  case timerStatus of 
    None -> button [onClick(ShowAddTimer "5")] [text "+ timer"]

    Adding maybeIntInput -> span [] [
                        input [size 2, maxlength 10, value maybeIntInput.input, onInput ShowAddTimer ] []
                        ,text " min "
                        ,button [disabled (maybeIntInput.maybeInt == Nothing), onClick (StartTimerMin maybeIntInput.maybeInt)] [text("start")]
                      ]

    Going _ -> button [onClick ResetTimer] [text "reset timer"]

-- view hjelpers
setColorButton: Color -> Html Msg
setColorButton color = 
  button [onClick (SetColor color)] [text(colorString color)]

timePartToString part = 
  part 
  |> String.fromInt
  |> String.pad 2 '0'    


