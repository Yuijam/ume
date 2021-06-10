module Gua exposing (..)

import Element exposing (Element, alignLeft, alignRight, centerX, column, el, fill, height, layout, none, padding, px, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (math)


type alias Model =
    ( Int, Int, Int )


type YaoUnit
    = Yin
    | Yang


type Yao
    = Normal YaoUnit
    | DongYao YaoUnit


type alias Gua =
    ( Yao, Yao, Yao )


type alias GuaCfg =
    { idx : Int
    , text : String
    , gua : Gua
    }


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


yaoView : Yao -> Element.Element msg
yaoView yao =
    case yao of
        Normal Yin ->
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

        DongYao Yin ->
            row [ width fill ]
                [ el
                    [ Background.color (rgb255 255 0 0)
                    , width (px 16)
                    , height (px 10)
                    , alignLeft
                    ]
                    none
                , el
                    [ Background.color (rgb255 255 0 0)
                    , width (px 16)
                    , height (px 10)
                    , alignRight
                    ]
                    none
                ]

        Normal Yang ->
            row [ width fill ]
                [ el
                    [ Background.color (rgb255 0 0 0)
                    , width fill
                    , height (px 10)
                    ]
                    none
                ]

        DongYao Yang ->
            row [ width fill ]
                [ el
                    [ Background.color (rgb255 255 0 0)
                    , width fill
                    , height (px 10)
                    ]
                    none
                ]


guaView : GuaCfg -> Element.Element msg
guaView guaCfg =
    let
        ( yao1, yao2, yao3 ) =
            guaCfg.gua
    in
    column [ width (px 46), spacing 5 ]
        [ yaoView yao1, yaoView yao2, yaoView yao3 ]


benGuaView : Model -> Element.Element msg
benGuaView ( shangGuaNum, xiaGuaNum, dongYao ) =
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


negate : YaoUnit -> YaoUnit
negate yaoUnit =
    case yaoUnit of
        Yang ->
            Yin

        Yin ->
            Yang


negateToDongYao : Yao -> Yao
negateToDongYao yao =
    case yao of
        Normal a ->
            DongYao <| negate a

        _ ->
            yao


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


toBianGua : GuaCfg -> GuaCfg -> Int -> ( GuaCfg, GuaCfg )
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
bianGuaView ( shangGuaNum, xiaGuaNum, dongYao ) =
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
                    toBianGua shangGua xiaGua dongYao
            in
            column [ width (px 46), spacing 10 ]
                [ guaView shang, guaView xia ]

        _ ->
            none


toHuGua : GuaCfg -> GuaCfg -> ( GuaCfg, GuaCfg )
toHuGua shangGuaCfg xiaGuaCfg =
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
huGuaView ( shangGuaNum, xiaGuaNum, dongYao ) =
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
                    toHuGua shangGua xiaGua
            in
            column [ width (px 46), spacing 10 ]
                [ guaView shang, guaView xia ]

        _ ->
            none


view : Model -> Element.Element msg
view model =
    row [ spacing 20, centerX ]
        [ benGuaView model
        , huGuaView model
        , bianGuaView model
        ]
