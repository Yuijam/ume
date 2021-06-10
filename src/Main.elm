module Main exposing (main)

import Browser
import Element exposing (Element, alignRight, centerX, column, el, layout, padding, rgb255, spacing, text)
import Element.Border as Border
import Element.Input as Input
import Gua
import Html exposing (Html)
import Html.Attributes exposing (placeholder)
import Html.Events exposing (onClick, onInput)


type alias Model =
    { inputMod : InputModel
    , guaMod : Gua.Model
    }


type alias InputModel =
    { firstNum : String, secondNum : String, thirdNum : String, result : String }


type ChangeArea
    = FirstNum
    | SecondNum
    | ThirdNum


type Msg
    = CalcResult
    | Change ChangeArea String


init : Model
init =
    { inputMod = { firstNum = "", secondNum = "", thirdNum = "", result = "" }
    , guaMod = ( -1, -1, -1 )
    }


inputNumToGuaNum : String -> Int -> Int
inputNumToGuaNum inputNum modNum =
    case String.toInt inputNum of
        Just n ->
            modBy modNum n

        Nothing ->
            -1


toDongYaoNum : String -> Int
toDongYaoNum dongYao =
    let
        n =
            inputNumToGuaNum dongYao 6
    in
    if n == 0 then
        6

    else
        n


update : Msg -> Model -> Model
update msg ({ inputMod, guaMod } as model) =
    case msg of
        CalcResult ->
            let
                shangGuaNum =
                    inputNumToGuaNum inputMod.firstNum 8

                xiaGuaNum =
                    inputNumToGuaNum inputMod.secondNum 8

                dongYaoNum =
                    toDongYaoNum inputMod.thirdNum
            in
            { model
                | guaMod = ( shangGuaNum, xiaGuaNum, dongYaoNum )
                , inputMod =
                    { inputMod
                        | result =
                            String.fromInt shangGuaNum
                                ++ " "
                                ++ String.fromInt xiaGuaNum
                                ++ " "
                                ++ String.fromInt dongYaoNum
                    }
            }

        Change area text ->
            case area of
                FirstNum ->
                    { model | inputMod = { inputMod | firstNum = text } }

                SecondNum ->
                    { model | inputMod = { inputMod | secondNum = text } }

                ThirdNum ->
                    { model | inputMod = { inputMod | thirdNum = text } }



-- renderlist : List String -> (String -> Msg) -> Html Msg
-- renderlist list click =
--     list |> List.map (\l -> li [ onClick (click l) ] [ text l ]) |> Html.form []
-- view : Model -> Html Msg
-- view model =
--     div []
--         [ input [ placeholder "first", value model.firstNum, onInput <| Change FirstNum ] []
--         , input [ placeholder "second", value model.secondNum, onInput (Change SecondNum) ] []
--         , input [ placeholder "third", value model.thirdNum, onInput (Change ThirdNum) ] []
--         , button [ onClick CalcResult ] [ text "Result" ]
--         , h3 [] [ text model.result ]
--         ]


inputView : InputModel -> Element Msg
inputView model =
    column [ spacing 3 ]
        [ Input.text []
            { onChange = Change FirstNum
            , label = Input.labelLeft [] (text "上卦")
            , text = model.firstNum
            , placeholder = Nothing
            }
        , Input.text []
            { onChange = Change SecondNum
            , label = Input.labelLeft [] (text "下卦")
            , text = model.secondNum
            , placeholder = Nothing
            }
        , Input.text []
            { onChange = Change ThirdNum
            , label = Input.labelLeft [] (text "动爻")
            , text = model.thirdNum
            , placeholder = Nothing
            }
        , Input.button
            [ padding 5
            , alignRight
            , Border.width 1
            , Border.rounded 3
            , Border.color <| rgb255 200 200 200
            ]
            { onPress = Just CalcResult
            , label = text "生成"
            }
        , text model.result
        ]


view : Model -> Html Msg
view model =
    layout [] <|
        column [ centerX ]
            [ inputView model.inputMod
            , Gua.view model.guaMod
            ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
