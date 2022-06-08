module Core.Style.StyleSheet exposing (css)

import Css exposing (..)



{- BEM naming convention is used -}


css =
    stylesheet
        [ class "experiment"
            [ displayFlex
            , flexDirection column
            , flexWrap initial
            , height (pct 100)
            , flex (num 1)
            , boxSizing borderBox
            ]
        , class "experiment__simulator"
            [ flexGrow (num 1)
            , flexShrink (num 1)
            , flexBasis auto
            , displayFlex
            , flexDirection column
            ]
        , class "experiment__history"
            [ flexGrow (num 0)
            , flexShrink (num 1)
            , flexBasis auto
            , padding (px 4)
            , margin (px 4)
            , displayFlex
            , justifyContent flexEnd
            ]
        , class "experiment-container"
            [ flexGrow (num 1)
            , flexShrink (num 1)
            , flexBasis auto
            , displayFlex
            , flexDirection column
            ]
        , class "feedback-container"
            [ flex (num 1)
            , paddingLeft (px 16)
            , paddingRight (px 16)
            , margin (px 4)
            , displayFlex
            , justifyContent spaceAround
            , alignItems center
            ]
        , class "observables-container"
            [ flex2 (num 7) (num 0)
            , margin (px 4)
            , displayFlex
            , justifyContent center
            , alignItems center
            ]
        , class "controls-container"
            [ flex (num 1)
            , paddingLeft (px 16)
            , paddingRight (px 16)
            , margin (px 4)
            , displayFlex
            , justifyContent start
            , alignItems center
            , flexWrap wrap
            ]
        , class "button__action--primary"
            [ disabled
                [ opacity (num 0.65)
                , cursor default
                ]
            , hover
                [ backgroundImage (linearGradient2 toBottom (stop <| rgb 144 202 249) (stop <| rgb 95 178 247) [])
                ]
            , color (rgb 0 0 0)
            , backgroundColor (rgb 144 202 249)
            , display inlineFlex
            , padding2 (rem 0.5) (rem 1)
            , border3 (px 1) solid (rgb 95 170 247)
            , margin (px 4)
            , minWidth (em 5)
            , fontFamily monospace
            , borderRadius (rem 0.5)
            , property "transition" "all 0.15s ease-in-out 0s"
            , alignItems center
            , justifyContent center
            , cursor pointer
            ]
        , class "button__action--secondary"
            [ disabled
                [ opacity (num 0.65)
                , cursor default
                , hover
                    [ opacity (num 0.5)
                    ]
                ]
            , hover
                [ backgroundColor (rgb 144 202 249)
                , color (rgb 0 0 0)
                ]
            , color (rgb 0 0 0)
            , backgroundColor (rgb 255 255 255)
            , display inlineFlex
            , padding2 (rem 0.5) (rem 1)
            , margin (px 4)
            , border3 (px 1) solid (rgb 95 170 247)
            , minWidth (em 5)
            , fontFamily monospace
            , borderRadius (rem 0.5)
            , property "transition" "all 0.15s ease-in-out 0s"
            , alignItems center
            , justifyContent center
            , cursor pointer
            ]
        , class "svg-icon"
            [ marginRight (em 0.5)
            , height (em 1.33)
            ]
        , class "prompt--success"
            [ displayFlex
            , justifyContent center
            , alignItems center
            , fontFamily sansSerif
            , flexGrow (num 1)
            , color (hex "#155724")
            , backgroundColor (hex "#d4edda")
            , borderColor (hex "#c3e6cb")
            , fontSize (em 1.3)
            , boxSizing borderBox
            , border2 (px 1) solid
            , borderRadius (rem 0.25)
            , padding2 (rem 0.75) (rem 1.25)
            , margin (rem 0.5)
            ]
        , class "prompt--danger"
            [ displayFlex
            , justifyContent center
            , alignItems center
            , fontFamily sansSerif
            , flexGrow (num 1)
            , color (hex "#721c24")
            , backgroundColor (hex "#f8d7da")
            , borderColor (hex "#f5c6cb")
            , fontSize (em 1.3)
            , boxSizing borderBox
            , border2 (px 1) solid
            , borderRadius (rem 0.25)
            , padding2 (rem 0.75) (rem 1.25)
            , margin (rem 0.5)
            ]
        , class "prompt--info"
            [ displayFlex
            , justifyContent center
            , alignItems center
            , fontFamily sansSerif
            , flexGrow (num 1)
            , color (hex "#004085")
            , backgroundColor (hex "#cce5ff")
            , borderColor (hex "#b8daff")
            , fontSize (em 1.3)
            , boxSizing borderBox
            , border2 (px 1) solid
            , borderRadius (rem 0.25)
            , padding2 (rem 0.75) (rem 1.25)
            , margin (rem 0.5)
            ]
        , class "modal-bg"
            [ position absolute
            , displayFlex
            , top (px 0)
            , right (px 0)
            , bottom (px 0)
            , left (px 0)
            , justifyContent center
            , alignItems center
            , width (pct 100)
            , height (pct 100)
            ]
        , class "modal-encl"
            [ position relative
            , padding (rem 1)
            , width (pct 35)
            , maxHeight (pct 50)
            ]
        , class "modal-content"
            [ position relative
            , backgroundColor (rgb 230 230 230)
            , boxShadow5 (px 0) (px 1) (px 3) (px 0) (rgba 1 1 1 0.6)
            , borderRadius (rem 0.25)
            ]
        , class "modal-header"
            [ displayFlex
            , justifyContent spaceBetween
            , alignItems flexStart
            , padding (rem 1)
            , border3 (px 0) dashed (rgba 1 1 1 0.0)
            , borderBottom3 (px 1) solid (rgba 1 1 1 0.7)
            ]
        , class "modal-header"
            [ descendants
                [ everything
                    [ all unset ]
                ]
            ]
        , class "modal-body"
            [ padding (rem 1.5)
            ]
        , class "modal-footer"
            [ padding (rem 1.5)
            , justifyContent flexStart
            , alignItems center
            , border3 (px 0) dashed (rgba 1 1 1 0.0)
            , borderTop3 (px 1) solid (rgba 1 1 1 0.7)
            ]
        ]


primaryAccentColor =
    rgb 144 202 249
