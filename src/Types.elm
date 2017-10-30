module Types exposing (..)

import Http


type Msg
    = UpdateData (Result Http.Error ( String, SummaryData, List TestStory ))
    | ExpandStory Int


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
    }


type alias Step =
    { title : String
    , result : Int
    }


type alias Model =
    { test : String
    , summary : Maybe SummaryData
    , tests : List TestStory
    }
