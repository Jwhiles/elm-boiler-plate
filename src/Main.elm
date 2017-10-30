module Main exposing (..)

import Types exposing (..)
import Html exposing (..)
import Http exposing (..)
import View exposing (..)
import Json.Decode.Pipeline exposing (requiredAt, required, decode, hardcoded)
import Json.Decode as Json


-- main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , subscriptions = always Sub.none
        , update = update
        }



-- model


init : ( Model, Cmd Msg )
init =
    ( Model "" Nothing [], getJson )


getJson : Cmd Msg
getJson =
    let
        url =
            "./bddfy.json"

        request =
            Http.get url testDecoder
    in
        Http.send UpdateData request


testDecoder : Json.Decoder ( String, SummaryData, List TestStory )
testDecoder =
    decode (,,)
        |> requiredAt [ "RunDate" ] Json.string
        |> required "Summary" summaryDecoder
        |> required "Stories" (Json.list storyDecoder)


summaryDecoder : Json.Decoder SummaryData
summaryDecoder =
    decode SummaryData
        |> required "Namespaces" Json.int
        |> required "Scenarios" Json.int
        |> required "Stories" Json.int
        |> required "Passed" Json.int
        |> required "Failed" Json.int


stepDecoder : Json.Decoder Step
stepDecoder =
    decode Step
        |> required "Title" Json.string
        |> required "Result" Json.int


scenarioDecoder : Json.Decoder Scenario
scenarioDecoder =
    decode Scenario
        |> required "Title" Json.string
        |> required "Duration" Json.string
        |> required "Steps" (Json.list stepDecoder)
        |> hardcoded False


storyDecoder : Json.Decoder TestStory
storyDecoder =
    decode TestStory
        |> required "Namespace" Json.string
        |> required "Result" Json.int
        |> required "Scenarios" (Json.list scenarioDecoder)
        |> hardcoded False



--update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateData (Err error) ->
            ( model, Cmd.none )

        UpdateData (Ok ( string, summary, tests )) ->
            let
                failuresExpanded =
                    List.map openFailedTests tests
            in
                ( { model
                    | test = string
                    , summary = Just summary
                    , tests =
                        failuresExpanded
                  }
                , Cmd.none
                )

        ExpandStory index ->
            let
                tests =
                    List.indexedMap (toggleExpanded index) model.tests
            in
                ( { model | tests = tests }, Cmd.none )


openFailedTests : TestStory -> TestStory
openFailedTests test =
    if test.success == 1 then
        test
    else
        { test
            | expanded =
                True
        }


type alias Expandable a =
    { a | expanded : Bool }


toggleExpanded : Int -> Int -> Expandable a -> Expandable a
toggleExpanded indexToChange index test =
    if index == indexToChange then
        { test | expanded = not test.expanded }
    else
        test
