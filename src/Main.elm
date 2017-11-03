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
    ( Model "" Nothing [] [] [], getJson )


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
        |> required "Stories" (Json.list getNamespaceId)


summaryDecoder : Json.Decoder SummaryData
summaryDecoder =
    decode SummaryData
        |> required "Namespaces" Json.int
        |> required "Scenarios" Json.int
        |> required "Stories" Json.int
        |> required "Passed" Json.int
        |> required "Failed" Json.int


stepDecoder : String -> Json.Decoder Step
stepDecoder scenarioId =
    decode Step
        |> required "Title" Json.string
        |> required "Result" Json.int
        |> hardcoded scenarioId


getNamespaceId : Json.Decoder TestStory
getNamespaceId =
    Json.field "Namespace" Json.string
        |> Json.andThen storyDecoder


getScenarioId : String -> Json.Decoder Scenario
getScenarioId nameSpace =
    Json.field "Id" Json.string
        |> Json.andThen (scenarioDecoder nameSpace)


scenarioDecoder : String -> String -> Json.Decoder Scenario
scenarioDecoder nameSpace scenarioId =
    decode Scenario
        |> required "Title" Json.string
        |> required "Duration" Json.string
        |> required "Steps" (Json.list (stepDecoder scenarioId))
        |> hardcoded False
        |> hardcoded scenarioId
        |> hardcoded nameSpace


storyDecoder : String -> Json.Decoder TestStory
storyDecoder nameSpace =
    decode TestStory
        |> hardcoded nameSpace
        |> required "Result" Json.int
        |> required "Scenarios" (Json.list (getScenarioId nameSpace))
        |> hardcoded False


concatInside accessor x y =
    accessor x ++ y


normalise : List TestStory -> ( List TestStory, List Scenario, List Step )
normalise oldStructure =
    let
        nameSpaces =
            List.map (\x -> { x | scenarios = [] }) oldStructure

        scenarios =
            List.foldr (concatInside .scenarios) [] oldStructure

        tests =
            List.foldr (concatInside .steps) [] scenarios
    in
        ( nameSpaces, scenarios, tests )



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

                ( nameSpaces, scenarios, steps ) =
                    normalise tests
            in
                ( { model
                    | test = string
                    , summary = Just summary
                    , nameSpaces = nameSpaces
                    , scenarios = scenarios
                    , steps = steps
                  }
                , Cmd.none
                )

        ExpandScenario id ->
            let
                expanded =
                    List.map (expand .id id)
                        model.scenarios
            in
                ( { model | scenarios = expanded }, Cmd.none )

        ExpandStory nameSpace ->
            let
                expanded =
                    List.map (expand .nameSpace nameSpace)
                        model.nameSpaces
            in
                ( { model | nameSpaces = expanded }, Cmd.none )


type alias Expandable r =
    { r | expanded : Bool }


expand : (Expandable r -> a) -> a -> Expandable r -> Expandable r
expand accessor test x =
    if accessor x == test then
        { x | expanded = not x.expanded }
    else
        x


openFailedTests : TestStory -> TestStory
openFailedTests test =
    if test.success == 1 then
        test
    else
        { test
            | expanded =
                True
        }
