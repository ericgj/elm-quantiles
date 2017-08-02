module Quantiles exposing 
    ( quantiles
    , quantile
    , quantilesWith
    , quantileWith
    , quantilesR1
    , quantilesR2
    , quantilesR3
    , quantilesR4
    , quantilesR5
    , quantilesR6
    , quantilesR7
    , quantilesR8
    , quantilesR9
    , quantileR1
    , quantileR2
    , quantileR3
    , quantileR4
    , quantileR5
    , quantileR6
    , quantileR7
    , quantileR8
    , quantileR9
    , r1
    , r2
    , r3
    , r4
    , r5
    , r6
    , r7
    , r8
    , r9
    , sort
    )

{-| Calculate quantiles, using various rounding/interpolation functions 
(or providing your own).

# Sorting

Before calling any quantile function, you must guarantee it is a sorted
`List Float`.

@docs sort

# Single quantiles

Quantiles are specified as a percentile between 0 and 1.
Note an empty list of data results in `Nothing`.

@docs quantile, quantileWith, quantileR1, quantileR2, quantileR3, quantileR4, quantileR5, quantileR6, quantileR7, quantileR8, quantileR9


# Multiple quantiles

Quantiles are specified as a list of percentiles between 0 and 1, and results
are returned as a list matching the specified percentiles.

Note an empty list of data results in `Nothing`.

@docs quantiles, quantilesWith, quantilesR1, quantilesR2, quantilesR3, quantilesR4, quantilesR5, quantilesR6, quantilesR7, quantilesR8, quantilesR9


# Predefined interpolation functions

These correspond to the R language types 1 to 9. For implementation details, see 
[wikipedia](https://en.wikipedia.org/wiki/Quantile) and 
[R sample quantiles](https://www.rdocumentation.org/packages/stats/versions/3.4.1/topics/quantile) 
documentation.

@docs r1, r2, r3, r4, r5, r6, r7, r8, r9

-}

import Array exposing (Array)
import Sort exposing (TaggedList(..), Sorted)


{-| Sort a list of floats. Note you must pass your data into this function
before calculating quantiles.

    data |> sort |> quantilesR3 [0.25, 0.5, 0.75]

-}
sort : List Float -> TaggedList Float Sorted
sort = 
    Sort.sort


{-| Calculate a list of quantiles using the given _interpolation function_.

In most cases you will want to use one of the interpolation functions defined
by this module, named `r1` to `r9`, following the naming convention used by R.

See below, 
[Predefined interpolation functions](#predefined-interpolation-functions).
-}
quantilesWith : 
    (Float -> Array Float -> Maybe Float) 
    -> List Float 
    -> TaggedList Float Sorted 
    -> Maybe (List Float)
quantilesWith method ps (TaggedList values) =
    case values of
        [] ->
            Nothing

        first :: rest ->
            let
                arrayValues =
                    Array.fromList values
            in
                List.map (\p -> method (clamp 0 1 p) arrayValues) ps
                    |> combine


{-| Calculate a list of quantiles. Note: uses the R default interpolation 
function (R-7).
-}
quantiles : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantiles =
    quantilesWith r7


{-| Calculate a list of quantiles using the R-1 interpolation function.
-}
quantilesR1 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR1 =
    quantilesWith r1


{-| Calculate a list of quantiles using the R-2 interpolation function.
-}
quantilesR2 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR2 =
    quantilesWith r2


{-| Calculate a list of quantiles using the R-3 interpolation function.
-}
quantilesR3 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR3 =
    quantilesWith r3


{-| Calculate a list of quantiles using the R-4 interpolation function.
-}
quantilesR4 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR4 =
    quantilesWith r4


{-| Calculate a list of quantiles using the R-5 interpolation function.
-}
quantilesR5 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR5 =
    quantilesWith r5


{-| Calculate a list of quantiles using the R-6 interpolation function.
-}
quantilesR6 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR6 =
    quantilesWith r6


{-| Calculate a list of quantiles using the R-7 interpolation function.
-}
quantilesR7 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR7 =
    quantilesWith r7


{-| Calculate a list of quantiles using the R-8 interpolation function.
-}
quantilesR8 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR8 =
    quantilesWith r8


{-| Calculate a list of quantiles using the R-9 interpolation function.
-}
quantilesR9 : List Float -> TaggedList Float Sorted -> Maybe (List Float)
quantilesR9 =
    quantilesWith r9


{-| Calculate a single quantile using the given interpolation function.

    median = quantileWith r5 0.5 data
-}
quantileWith : 
    (Float -> Array Float -> Maybe Float) 
    -> Float 
    -> TaggedList Float Sorted 
    -> Maybe Float
quantileWith method p (TaggedList values) =
    case values of
        [] ->
            Nothing

        _ ->
            method (clamp 0 1 p) (Array.fromList values)


{-| Calculate a single quantile. Note: uses the R default interpolation 
function (R-7).
-}
quantile : Float -> TaggedList Float Sorted -> Maybe Float
quantile =
    quantileWith r7


{-| Calculate a single quantile using the R-1 interpolation function.
-}
quantileR1 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR1 =
    quantileWith r1


{-| Calculate a single quantile using the R-2 interpolation function.
-}
quantileR2 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR2 =
    quantileWith r2


{-| Calculate a single quantile using the R-3 interpolation function.
-}
quantileR3 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR3 =
    quantileWith r3


{-| Calculate a single quantile using the R-4 interpolation function.
-}
quantileR4 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR4 =
    quantileWith r4


{-| Calculate a single quantile using the R-5 interpolation function.
-}
quantileR5 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR5 =
    quantileWith r5


{-| Calculate a single quantile using the R-6 interpolation function.
-}
quantileR6 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR6 =
    quantileWith r6


{-| Calculate a single quantile using the R-7 interpolation function.
-}
quantileR7 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR7 =
    quantileWith r7


{-| Calculate a single quantile using the R-8 interpolation function.
-}
quantileR8 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR8 =
    quantileWith r8


{-| Calculate a single quantile using the R-9 interpolation function.
-}
quantileR9 : Float -> TaggedList Float Sorted -> Maybe Float
quantileR9 =
    quantileWith r9


{-| The R-1 interpolation function.
-}
r1 : Float -> Array Float -> Maybe Float
r1 p values =
    if p == 0 then
        Array.get 0 values
    else
        let
            h =
                (p * (toFloat (Array.length values))) + 0.5

            interpolate () =
                Array.get ((ceiling (h - 0.5)) - 1) values
        in
            getOrInterpolate interpolate h values


{-| The R-2 interpolation function.
-}
r2 : Float -> Array Float -> Maybe Float
r2 p values =
    if p == 0.0 then
        Array.get 0 values
    else if p == 1.0 then
        Array.get ((Array.length values) - 1) values
    else
        let
            h =
                (p * (toFloat (Array.length values))) + 0.5

            interpolate () =
                let
                    x0 =
                        Array.get ((ceiling (h - 0.5)) - 1) values

                    x1 =
                        Array.get ((ceiling (h + 0.5)) - 1) values
                in
                    Maybe.map2 (\a b -> (a + b) / 2) x0 x1
        in
            getOrInterpolate interpolate h values


{-| The R-3 interpolation function.
-}
r3 : Float -> Array Float -> Maybe Float
r3 p values =
    if p <= (0.5 / (toFloat (Array.length values))) then
        Array.get 0 values
    else
        let
            h =
                p * (toFloat (Array.length values))

            interpolate () =
                Array.get ((round h) - 1) values
        in
            getOrInterpolate interpolate h values


{-| The R-4 interpolation function.
-}
r4 : Float -> Array Float -> Maybe Float
r4 p values =
    if p < (1 / (toFloat (Array.length values))) then
        Array.get 0 values
    else if p == 1.0 then
        Array.get ((Array.length values) - 1) values
    else
        let
            h =
                p * (toFloat (Array.length values))

            interpolate () =
                let
                    x0 =
                        Array.get ((floor h) - 1) values

                    x1 =
                        Array.get (floor h) values

                    hdiff =
                        h - (toFloat (floor h))
                in
                    Maybe.map2 (\a b -> a + (hdiff * (b - a)))
                        x0
                        x1
        in
            getOrInterpolate interpolate h values


{-| The R-5 interpolation function.
-}
r5 : Float -> Array Float -> Maybe Float
r5 p values =
    let
        n =
            Array.length values
    in
        if p < (0.5 / (toFloat n)) then
            Array.get 0 values
        else if p >= (((toFloat n) - 0.5) / (toFloat n)) then
            Array.get (n - 1) values
        else
            let
                h =
                    (p * (toFloat n)) + 0.5

                interpolate () =
                    linearInterpolate h values
            in
                getOrInterpolate interpolate h values


{-| The R-6 interpolation function.
-}
r6 : Float -> Array Float -> Maybe Float
r6 p values =
    let
        n =
            Array.length values
    in
        if p < (1 / (toFloat (n + 1))) then
            Array.get 0 values
        else if p >= ((toFloat n) / (toFloat (n + 1))) then
            Array.get (n - 1) values
        else
            let
                h =
                    (p * (toFloat (n + 1)))

                interpolate () =
                    linearInterpolate h values
            in
                getOrInterpolate interpolate h values


{-| The R-7 interpolation function.
-}
r7 : Float -> Array Float -> Maybe Float
r7 p values =
    let
        n =
            Array.length values
    in
        if p == 1 then
            Array.get (n - 1) values
        else
            let
                h =
                    (p * (toFloat (n - 1))) + 1

                interpolate () =
                    linearInterpolate h values
            in
                getOrInterpolate interpolate h values


{-| The R-8 interpolation function.
-}
r8 : Float -> Array Float -> Maybe Float
r8 p values =
    let
        n =
            Array.length values

        nfloat =
            toFloat n
    in
        if p < ((2 / 3) / (nfloat + (1 / 3))) then
            Array.get 0 values
        else if p >= ((nfloat - (1 / 3)) / (nfloat + (1 / 3))) then
            Array.get (n - 1) values
        else
            let
                h =
                    (p * (nfloat + (1 / 3))) + (1 / 3)

                interpolate () =
                    linearInterpolate h values
            in
                getOrInterpolate interpolate h values


{-| The R-9 interpolation function.
-}
r9 : Float -> Array Float -> Maybe Float
r9 p values =
    let
        n =
            Array.length values

        nfloat =
            toFloat n
    in
        if p < ((5 / 8) / (nfloat + (1 / 4))) then
            Array.get 0 values
        else if p >= ((nfloat - (3 / 8)) / (nfloat + (1 / 4))) then
            Array.get (n - 1) values
        else
            let
                h =
                    (p * (nfloat + (1 / 4))) + (3 / 8)

                interpolate () =
                    linearInterpolate h values
            in
                getOrInterpolate interpolate h values


linearInterpolate : Float -> Array Float -> Maybe Float
linearInterpolate h values =
    let
        x0 =
            Array.get ((floor h) - 1) values

        x1 =
            Array.get (floor h) values

        hdiff =
            h - (toFloat (floor h))
    in
        Maybe.map2 (\a b -> a + (hdiff * (b - a)))
            x0
            x1


getOrInterpolate : (() -> Maybe Float) -> Float -> Array Float -> Maybe Float
getOrInterpolate fn h values =
    if h - (toFloat (truncate h)) == 0.0 then
        Array.get ((truncate h) - 1) values
    else
        fn ()



-- Note: Copied from List.Extra


traverse : (a -> Maybe b) -> List a -> Maybe (List b)
traverse f =
    let
        step e acc =
            case f e of
                Nothing ->
                    Nothing

                Just x ->
                    Maybe.map ((::) x) acc
    in
        List.foldr step (Just [])


combine : List (Maybe a) -> Maybe (List a)
combine =
    traverse identity
