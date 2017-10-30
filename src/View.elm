module View exposing (view)

import Html exposing (..)
import Types exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class)


resultFromInt : Int -> Bool
resultFromInt i =
    case i of
        1 ->
            True

        _ ->
            False


storyView : Int -> TestStory -> Html Msg
storyView index testStory =
    let
        styles =
            if (index % 2 == 0) then
                "fl w-100 tl bn bg-black-05"
            else
                "fl w-100 tl bn bg-black-10"

        content =
            if testStory.expanded == True then
                List.map scenarioView testStory.scenarios
            else
                []
    in
        div []
            [ button [ onClick (ExpandStory index), class styles ]
                [ h3 [] [ text testStory.nameSpace ]
                ]
            , div [ class styles ] content
            ]


storiesView : List TestStory -> Html Msg
storiesView stories =
    div [] <| List.indexedMap storyView stories


scenarioView : Scenario -> Html Msg
scenarioView scenario =
    div []
        [ h4 [] [ text scenario.title ]
        , p []
            [ text scenario.duration ]
        , div
            []
          <|
            List.indexedMap stepView scenario.steps
        ]


stepView : Int -> Step -> Html Msg
stepView index step =
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
        , storiesView model.tests
        ]
