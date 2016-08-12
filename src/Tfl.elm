module Tfl exposing
  ( Prediction
  , getPredictions
  , StopPoint
  , getStopPoint
  )


{-| This is a wrapper for the TfL (Transport for London) public API.

The API is described at https://api.tfl.gov.uk and https://api-portal.tfl.gov.uk/docs.

# Predictions

@docs Prediction, getPredictions

# StopPoint

@docs StopPoint, getStopPoints

-}


import Date
import Http
import Json.Decode exposing ((:=))
import Task
import Time


apiEndpoint = "https://api.tfl.gov.uk"


type alias Prediction =
  { destinationName : String
  , expectedArrival : Time.Time
  , lineName : String
  , platformName : String
  , stationName : String
  , towards : String
  , modeName : String
  , timestamp : Time.Time
  }


(<$>) : (a -> b) -> Json.Decode.Decoder a -> Json.Decode.Decoder b
(<$>) =
    Json.Decode.map


(<*>) : Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder a -> Json.Decode.Decoder b
(<*>) f v =
    f `Json.Decode.andThen` \x -> x <$> v


predictionDecoder : Json.Decode.Decoder Prediction
predictionDecoder =
  Prediction
    <$> ("destinationName" := Json.Decode.string)
    <*> ("expectedArrival" := timeDecoder)
    <*> ("lineName" := Json.Decode.string)
    <*> ("platformName" := Json.Decode.string)
    <*> ("stationName" := Json.Decode.string)
    <*> ("towards" := Json.Decode.string)
    <*> ("modeName" := Json.Decode.string)
    <*> ("timestamp" := timeDecoder)


timeDecoder : Json.Decode.Decoder Time.Time
timeDecoder =
  Json.Decode.customDecoder
    Json.Decode.string
    Date.fromString
    |> Json.Decode.map (Date.toTime)


{-| Get the list of arrival predictions for given line id.

See:

- https://api.tfl.gov.uk/#StopPoint
- https://api.tfl.gov.uk/#/StopPoint/{id}/Arrivals .
-}
getPredictions : String -> Task.Task Http.Error (List Prediction)
getPredictions stopId =
  Http.get
    (Json.Decode.list predictionDecoder)
    ("https://api.tfl.gov.uk/StopPoint/" ++ stopId ++ "/arrivals")


type alias StopPoint =
  { commonName : String
  , latitude : Float
  , longitude : Float
  , modes : List String
  }


stopPointDecoder : Json.Decode.Decoder StopPoint
stopPointDecoder =
  StopPoint
    <$> ("commonName" := Json.Decode.string)
    <*> ("lat" := Json.Decode.float)
    <*> ("lon" := Json.Decode.float)
    <*> ("modes" := Json.Decode.list Json.Decode.string)


{-| Get the StopPoint corresponding to the given stop id.

See:

- https://api.tfl.gov.uk/#StopPoint
- https://api.tfl.gov.uk/#/StopPoint/{id} .
-}
getStopPoint : String -> Task.Task Http.Error StopPoint
getStopPoint stopId =
  Http.get
    stopPointDecoder
    ("https://api.tfl.gov.uk/StopPoint/" ++ stopId)
