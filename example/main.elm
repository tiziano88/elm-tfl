import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import String
import Task
import Time


import Tfl


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
  { predictions : List Tfl.Prediction
  , stationName : String
  , platformName : String
  , now : Time.Time
  }


init : (Model, Cmd Msg)
init =
  { predictions = []
  , stationName = ""
  , platformName = ""
  , now = 0.0
  }
  ! [ getPredictions ]


getPredictions : Cmd Msg
getPredictions =
  Task.perform (always Nop) GetPredictions (Tfl.getPredictions "490013767A")


-- UPDATE


type Msg
  = Nop
  | UpdatePredictions (Time.Time)
  | UpdateNow (Time.Time)
  | GetPredictions (List Tfl.Prediction)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Nop ->
      model ! []

    UpdatePredictions _ ->
      model ! [ getPredictions ]

    UpdateNow t ->
      let
        now = model.now
      in
        { model
        | now = now + Time.second
        } ! []

    GetPredictions predictions ->
      let
        f = List.head predictions
        now = f
          |> Maybe.map .timestamp
          |> Maybe.withDefault 0.0
        stationName = f
          |> Maybe.map .stationName
          |> Maybe.withDefault ""
        platformName = f
          |> Maybe.map .platformName
          |> Maybe.withDefault ""
      in
        { model
        | predictions = predictions
        , now = now
        , stationName = stationName
        , platformName = platformName
        } ! []


-- VIEW


view : Model -> Html Msg
view model =
  div
    [ style
      [ ("font-family", "'Work Sans', sans-serif")
      , ("text-align", "center")
      ]
    ]
    [ node "link"
      [ href "https://fonts.googleapis.com/css?family=Fira+Mono|Work+Sans:400,700"
      , rel "stylesheet"
      ]
      []
    , h1 [] [ text "TfL API Demo" ]
    , h2 []
      [ stationName model.stationName
      , platformName model.platformName
      ]
    , ul
      [ style
        [ ("list-style", "none")
        , ("padding", "0")
        ]
      ]
      (List.map (\x -> li [] [ viewPrediction model.now x ] ) (List.sortBy .expectedArrival model.predictions))
    ]


viewPrediction : Time.Time -> Tfl.Prediction -> Html a
viewPrediction now p =
  let
    interval = p.expectedArrival - now
  in
    div
      [ style
        [ ("height", "40px")
        ]
      ]
      [ remainingTime <| (minutesOnly interval) ++ ":" ++ (secondsOnly interval)
      , lineName p.lineName
      , destinationName p.destinationName
      ]


borderColor : String
borderColor = "red"


stationName : String -> Html a
stationName p =
  span
    [ style
      [ ("padding", "10px")
      , ("border-style", "solid")
      , ("border-color", borderColor)
      , ("border-top-left-radius", "30px")
      , ("border-bottom-left-radius", "30px")
      , ("border-right", "none")
      , ("padding-right", "30px")
      , ("height", "30px")
      , ("display", "inline-block")
      , ("text-align", "center")
      , ("margin", "5px")
      , ("margin-right", "-30px")
      ]
    ]
    [ text p ]



platformName : String -> Html a
platformName p =
  span
    [ style
      [ ("background-color", borderColor)
      , ("color", "white")
      , ("padding", "10px")
      , ("border-style", "solid")
      , ("border-color", borderColor)
      , ("border-radius", "30px")
      , ("width", "30px")
      , ("height", "30px")
      , ("display", "inline-block")
      , ("text-align", "center")
      , ("margin", "5px")
      ]
    ]
    [ text p ]


lineName : String -> Html a
lineName name =
  span
    [ style
      [ ("background-color", borderColor)
      , ("color", "white")
      , ("padding", "2px")
      , ("border-style", "solid")
      , ("border-color", borderColor)
      , ("border-top-left-radius", "5px")
      , ("border-bottom-left-radius", "5px")
      , ("font-weight", "bold")
      , ("width", "2em")
      , ("display", "inline-block")
      , ("text-align", "center")
      ]
    ]
    [ text name ]


destinationName : String -> Html a
destinationName name =
  span
    [ style
      [ ("padding", "2px")
      , ("border-style", "solid")
      , ("border-color", borderColor)
      , ("border-top-right-radius", "5px")
      , ("border-bottom-right-radius", "5px")
      , ("width", "10em")
      , ("display", "inline-block")
      , ("text-align", "center")
      ]
    ]
    [ text name ]


remainingTime : String -> Html a
remainingTime t =
  span
    [ style
      [ ("padding", "2px")
      , ("margin", "2px")
      , ("border-style", "solid")
      , ("border-color", "black")
      , ("border-radius", "5px")
      , ("width", "3em")
      , ("display", "inline-block")
      , ("text-align", "right")
      , ("font-family", "'Fira Mono', monospace")
      ]
    ]
    [ text t ]


minutesOnly : Time.Time -> String
minutesOnly t =
  toString <| (truncate (t / Time.second)) // 60


secondsOnly : Time.Time -> String
secondsOnly t =
  String.padLeft 2 '0' <| toString <| (truncate (t / Time.second)) % 60


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Time.every (5 * Time.second) UpdatePredictions
    , Time.every Time.second UpdateNow
    ]
