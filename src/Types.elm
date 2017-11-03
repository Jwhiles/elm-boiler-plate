module Types exposing (..)

import Http


type Msg
    = UpdateData (Result Http.Error ( String, SummaryData, List TestStory ))
    | ExpandStory String
    | ExpandScenario String


type alias SummaryData =
    { nameSpaces : Int
    , scenarios : Int
    , stories : Int
    , passed : Int
    , failed : Int
    }


type alias TestStory =
    { nameSpace : String
    , success : Int
    , scenarios : List Scenario
    , expanded : Bool
    }


type alias Scenario =
    { title : String
    , duration : String
    , steps : List Step
    , expanded : Bool
    , id : String
    , nameSpace : String
    }


type alias Step =
    { title : String
    , result : Int
    , scenarioId : String
    }


type alias Model =
    { test : String
    , summary : Maybe SummaryData
    , nameSpaces : List TestStory
    , scenarios : List Scenario
    , steps : List Step
    }
