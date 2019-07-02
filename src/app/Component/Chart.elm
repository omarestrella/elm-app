module Component.Chart exposing (view)

import Array exposing (Array)
import Color exposing (Color)
import Path exposing (Path)
import Shape exposing (Arc, defaultPieConfig)
import TypedSvg exposing (circle, g, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


rgba255 : Int -> Int -> Int -> Float -> Color
rgba255 r g b a =
    Color.fromRgba { red = toFloat r / 255, green = toFloat g / 255, blue = toFloat b / 255, alpha = a }


colors : Array Color
colors =
    Array.fromList
        [ rgba255 31 119 180 0.5
        , rgba255 255 127 14 0.5
        , rgba255 44 159 44 0.5
        , rgba255 214 39 40 0.5
        , rgba255 148 103 189 0.5
        , rgba255 140 86 75 0.5
        , rgba255 227 119 194 0.5
        , rgba255 128 128 128 0.5
        , rgba255 188 189 34 0.5
        , rgba255 23 190 207 0.5
        ]


radius : Float -> Float -> Float
radius w h =
    min (w / 2) h / 2


annular : List Arc -> Float -> Svg msg
annular arcs radius_ =
    let
        makeSlice index datum =
            Path.element (Shape.arc { datum | innerRadius = radius_ * 1.3 })
                [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors
                , stroke Color.black
                ]
    in
    g [ transform [ Translate (radius_ * 2) (radius_ * 2) ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        ]


view : List Float -> Float -> Float -> Svg msg
view model width height =
    let
        pieData =
            model |> Shape.pie { defaultPieConfig | outerRadius = radius width height * 1.9, cornerRadius = 5 }
    in
    svg [ viewBox 0 0 width height ]
        [ annular pieData (radius width height)
        ]
