module Gua exposing (..)

import Element exposing (Element, alignLeft, alignRight, column, el, fill, height, none, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


type alias Model =
    ( Int, Int, Int )


type Yao
    = Yin
    | Yang


type alias Gua =
    ( Yao, Yao, Yao )


type alias GuaCfg =
    ( Int, String, Gua )


guaConfig : List GuaCfg
guaConfig =
    [ ( 1, "乾", ( Yang, Yang, Yang ) )
    , ( 2, "兑", ( Yin, Yang, Yang ) )
    , ( 3, "离", ( Yang, Yin, Yang ) )
    , ( 4, "震", ( Yin, Yin, Yang ) )
    , ( 5, "巽", ( Yang, Yang, Yin ) )
    , ( 6, "坎", ( Yin, Yang, Yin ) )
    , ( 7, "艮", ( Yang, Yin, Yin ) )
    , ( 0, "坤", ( Yin, Yin, Yin ) )
    ]


errorGuaCfg : GuaCfg
errorGuaCfg =
    ( -1, "error", ( Yang, Yang, Yang ) )


getGua : Int -> Maybe GuaCfg
getGua guaNum =
    List.filter (\( idx, guaStr, _ ) -> idx == guaNum) guaConfig |> List.head


yaoView : Yao -> Element.Element msg
yaoView yao =
    case yao of
        Yin ->
            row [ width fill ]
                [ el
                    [ Background.color (rgb255 0 0 0)
                    , width (px 16)
                    , height (px 10)
                    , alignLeft
                    ]
                    none
                , el
                    [ Background.color (rgb255 0 0 0)
                    , width (px 16)
                    , height (px 10)
                    , alignRight
                    ]
                    none
                ]

        Yang ->
            row [ width fill ]
                [ el
                    [ Background.color (rgb255 0 0 0)
                    , width fill
                    , height (px 10)
                    ]
                    none
                ]


guaView : GuaCfg -> Element.Element msg
guaView ( idx, guaStr, ( yao1, yao2, yao3 ) ) =
    column [ width (px 46), spacing 5 ]
        [ yaoView yao1, yaoView yao2, yaoView yao3 ]


bieGuaView : Model -> Element.Element msg
bieGuaView ( shangGuaNum, xiaGuaNum, dongYao ) =
    let
        shangGuaCfg =
            getGua shangGuaNum

        xiaGuaCfg =
            getGua xiaGuaNum
    in
    case ( shangGuaCfg, xiaGuaCfg ) of
        ( Just shangGua, Just xiaGua ) ->
            column [ width (px 46), spacing 10 ]
                [ guaView shangGua, guaView xiaGua ]

        _ ->
            none


bianGuaView : Model -> Element.Element msg
bianGuaView ( shangGuaNum, xiaGuaNum, dongYao ) =
    let
        shangGuaCfg =
            getGua shangGuaNum

        xiaGuaCfg =
            getGua xiaGuaNum
    in
    case ( shangGuaCfg, xiaGuaCfg ) of
        ( Just shangGua, Just xiaGua ) ->
            column [ width (px 46), spacing 10 ]
                [ guaView shangGua, guaView xiaGua ]

        _ ->
            none


view : Model -> Element.Element msg
view model =
    bieGuaView model
