module Yao exposing (Model(..), YaoUnit(..), negateToDongYao, yaoView)

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


type YaoUnit
    = Yin
    | Yang


type Model
    = Normal YaoUnit
    | DongYao YaoUnit


negate : YaoUnit -> YaoUnit
negate yaoUnit =
    case yaoUnit of
        Yang ->
            Yin

        Yin ->
            Yang


negateToDongYao : Model -> Model
negateToDongYao yao =
    case yao of
        Normal a ->
            DongYao <| negate a

        _ ->
            yao


unitView : YaoUnit -> ( Int, Int, Int ) -> Element.Element msg
unitView yaoUnit ( r, g, b ) =
    case yaoUnit of
        Yang ->
            row [ width fill ]
                [ el
                    [ Background.color (rgb255 r g b)
                    , width fill
                    , height (px 10)
                    ]
                    none
                ]

        Yin ->
            row [ width fill ]
                [ el
                    [ Background.color (rgb255 r g b)
                    , width (px 16)
                    , height (px 10)
                    , alignLeft
                    ]
                    none
                , el
                    [ Background.color (rgb255 r g b)
                    , width (px 16)
                    , height (px 10)
                    , alignRight
                    ]
                    none
                ]


yaoView : Model -> Element.Element msg
yaoView yao =
    case yao of
        Normal a ->
            unitView a ( 0, 0, 0 )

        DongYao a ->
            unitView a ( 255, 0, 0 )
