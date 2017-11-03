module View exposing (view)

import Html exposing (..)
import Types exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)


scenariosByNamespace : Model -> TestStory -> List Scenario
scenariosByNamespace model story =
    List.filter (\x -> x.nameSpace == story.nameSpace) model.scenarios


stepsById : Model -> Scenario -> List Step
stepsById model scenario =
    List.filter (\x -> x.scenarioId == scenario.id) model.steps


resultFromInt : Int -> Bool
resultFromInt i =
    case i of
        1 ->
            True

        _ ->
            False


storyView : Model -> TestStory -> Html Msg
storyView model testStory =
    let
        styles =
            "fl w-100 tl bn bg-black-10"

        content =
            if testStory.expanded == True then
                List.map (scenarioView model) (scenariosByNamespace model testStory)
            else
                []
    in
        div []
            [ button [ onClick (ExpandStory testStory.nameSpace), class styles ]
                [ h3 [] [ text testStory.nameSpace ]
                ]
            , div [ class styles ] content
            ]


storiesView : Model -> Html Msg
storiesView model =
    div [] <| List.map (storyView model) model.nameSpaces


scenarioView : Model -> Scenario -> Html Msg
scenarioView model scenario =
    let
        content =
            if scenario.expanded == True then
                List.map stepView (stepsById model scenario)
            else
                []
    in
        button [ onClick (ExpandScenario scenario.id) ]
            [ h4 [] [ text scenario.title ]
            , p []
                [ text scenario.duration ]
            , div
                []
                content
            ]


stepView : Step -> Html Msg
stepView step =
    let
        styles =
            if resultFromInt step.result then
                "bg-green pa2 code f5"
            else
                "bg-red pa2 code f5"
    in
        div [ class styles ]
            [ h5 []
                [ text step.title ]
            ]


unitTests : Maybe SummaryData -> Html Msg
unitTests summary =
    case summary of
        Nothing ->
            div [] []

        Just data ->
            let
                percentagePassed =
                    floor ((toFloat data.passed / toFloat data.scenarios) * 100)

                backgroundColor =
                    if percentagePassed < 100 then
                        "bg-red"
                    else
                        "bg-green"
            in
                div [ class <| "fl h-100 w-100 tc w-50-ns " ++ backgroundColor ]
                    [ h2 [ class "code" ] [ text "unit tests" ]
                    , p [ class "code b f3" ] [ text <| (toString percentagePassed) ++ "%" ]
                    ]


steps : Maybe SummaryData -> Html Msg
steps summaryData =
    case summaryData of
        Nothing ->
            div [] []

        Just summary ->
            div [ class "fl h-100 w-100 tc w-50-ns bg-black-05 code" ]
                [ p [] [ text <| "total " ++ toString summary.scenarios ]
                , p [] [ text <| "passed " ++ toString summary.passed ]
                , p [] [ text <| "failed " ++ toString summary.failed ]
                ]


summary : Model -> Html Msg
summary model =
    let
        date =
            String.slice 0 10 model.test

        time =
            String.slice 11 19 model.test
    in
        div []
            [ div [ class "code pa3 pl1 b" ] [ text <| "Started at " ++ time ++ " on " ++ date ]
            ]


view : Model -> Html Msg
view model =
    div [ class "w-60-ns center" ]
        [ h1 [ class "code tc f1 i" ]
            [ text "Unit Tests for cool kids" ]
        , div
            [ class "cf h4" ]
            [ unitTests model.summary
            , steps model.summary
            ]
        , summary model
        , storiesView model
        ]
