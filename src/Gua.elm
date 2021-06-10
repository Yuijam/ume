module Gua exposing (..)

import Element
    exposing
        ( Element
        , alignLeft
        , alignRight
        , centerX
        , column
        , el
        , fill
        , height
        , layout
        , none
        , padding
        , px
        , rgb255
        , row
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Yao as Yao exposing (..)


type alias Model =
    ( Int, Int, Int )


type alias Gua =
    ( Yao.Model, Yao.Model, Yao.Model )


type alias GuaCfg =
    { idx : Int
    , text : String
    , gua : Gua
    }


type alias GuaConverter =
    GuaCfg -> GuaCfg -> Int -> ( GuaCfg, GuaCfg )


guaConfig : List GuaCfg
guaConfig =
    [ GuaCfg 1 "乾" ( Normal Yang, Normal Yang, Normal Yang )
    , GuaCfg 2 "兑" ( Normal Yin, Normal Yang, Normal Yang )
    , GuaCfg 3 "离" ( Normal Yang, Normal Yin, Normal Yang )
    , GuaCfg 4 "震" ( Normal Yin, Normal Yin, Normal Yang )
    , GuaCfg 5 "巽" ( Normal Yang, Normal Yang, Normal Yin )
    , GuaCfg 6 "坎" ( Normal Yin, Normal Yang, Normal Yin )
    , GuaCfg 7 "艮" ( Normal Yang, Normal Yin, Normal Yin )
    , GuaCfg 0 "坤" ( Normal Yin, Normal Yin, Normal Yin )
    ]


getGua : Int -> Maybe GuaCfg
getGua guaNum =
    List.filter (\{ idx, text, gua } -> idx == guaNum) guaConfig |> List.head


guaView : GuaCfg -> Element.Element msg
guaView guaCfg =
    let
        ( yao1, yao2, yao3 ) =
            guaCfg.gua
    in
    column [ width (px 46), spacing 5 ]
        [ yaoView yao1, yaoView yao2, yaoView yao3 ]


benGuaView : Model -> Element.Element msg
benGuaView model =
    bieGuaView model (\a b c -> ( a, b ))


changeGuaCfgByDongYao : GuaCfg -> Int -> GuaCfg
changeGuaCfgByDongYao guaCfg dongYao =
    if dongYao > 2 || dongYao < 0 then
        guaCfg

    else
        let
            ( yao1, yao2, yao3 ) =
                guaCfg.gua
        in
        if dongYao == 1 then
            { guaCfg | gua = ( yao1, yao2, negateToDongYao yao3 ) }

        else if dongYao == 2 then
            { guaCfg | gua = ( yao1, negateToDongYao yao2, yao3 ) }

        else
            { guaCfg | gua = ( negateToDongYao yao1, yao2, yao3 ) }


bieGuaView : Model -> GuaConverter -> Element.Element msg
bieGuaView ( shangGuaNum, xiaGuaNum, dongYao ) converter =
    let
        shangGuaCfg =
            getGua shangGuaNum

        xiaGuaCfg =
            getGua xiaGuaNum
    in
    case ( shangGuaCfg, xiaGuaCfg ) of
        ( Just shangGua, Just xiaGua ) ->
            let
                ( shang, xia ) =
                    converter shangGua xiaGua dongYao
            in
            column [ width (px 46), spacing 10 ]
                [ guaView shang, guaView xia ]

        _ ->
            none


toBianGua : GuaConverter
toBianGua shangGuaCfg xiaGuaCfg dongYao =
    let
        isShangDong =
            dongYao > 3

        dongIdx =
            modBy 3 dongYao
    in
    if isShangDong then
        ( changeGuaCfgByDongYao shangGuaCfg dongIdx, xiaGuaCfg )

    else
        ( shangGuaCfg, changeGuaCfgByDongYao xiaGuaCfg dongIdx )


bianGuaView : Model -> Element.Element msg
bianGuaView model =
    bieGuaView model toBianGua


toHuGua : GuaConverter
toHuGua shangGuaCfg xiaGuaCfg _ =
    let
        ( shangYao1, shangYao2, shangYao3 ) =
            shangGuaCfg.gua

        ( xiaYao1, xiaYao2, xiaYao3 ) =
            xiaGuaCfg.gua
    in
    ( { shangGuaCfg | gua = ( shangYao2, shangYao3, xiaYao1 ) }
    , { xiaGuaCfg | gua = ( shangYao3, xiaYao1, xiaYao2 ) }
    )


huGuaView : Model -> Element.Element msg
huGuaView model =
    bieGuaView model toHuGua


view : Model -> Element.Element msg
view model =
    row [ spacing 20, centerX ]
        [ benGuaView model
        , huGuaView model
        , bianGuaView model
        ]
