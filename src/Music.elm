module Music exposing (..)

import List.Extra
import Music.PitchClass as PitchClass exposing (PitchClass)
import Music.Scale as Scale exposing (Scale)
import Music.ScaleType as ScaleType exposing (ScaleType)



-- This section is a shame, but it's a whole in the Music Theory API


nextScaleType : ScaleType -> ScaleType
nextScaleType scaleType =
    if scaleType == ScaleType.major then
        ScaleType.minor

    else if scaleType == ScaleType.minor then
        ScaleType.harmonicMinor

    else if scaleType == ScaleType.harmonicMinor then
        ScaleType.major

    else
        ScaleType.major


prevScaleType : ScaleType -> ScaleType
prevScaleType scaleType =
    if scaleType == ScaleType.major then
        ScaleType.harmonicMinor

    else if scaleType == ScaleType.minor then
        ScaleType.major

    else if scaleType == ScaleType.harmonicMinor then
        ScaleType.minor

    else
        ScaleType.harmonicMinor


allPitchClasses : List PitchClass
allPitchClasses =
    [ PitchClass.c
    , PitchClass.cSharp
    , PitchClass.d
    , PitchClass.dSharp
    , PitchClass.e
    , PitchClass.f
    , PitchClass.fSharp
    , PitchClass.g
    , PitchClass.gSharp
    , PitchClass.a
    , PitchClass.aSharp
    , PitchClass.b
    ]


nextRoot : PitchClass -> PitchClass
nextRoot currentPitchClass =
    nextInList
        ((==) currentPitchClass)
        allPitchClasses
        PitchClass.c


prevRoot : PitchClass -> PitchClass
prevRoot currentPitchClass =
    prevInList
        ((==) currentPitchClass)
        allPitchClasses
        PitchClass.c


scaleTypeToString : ScaleType -> String
scaleTypeToString scaleType =
    if scaleType == ScaleType.major then
        "Major"

    else if scaleType == ScaleType.minor then
        "Minor"

    else if scaleType == ScaleType.harmonicMinor then
        "Harmonic Minor"

    else
        ""



-- TODO add more interesting scales
-- Can do pentatonic and blues
-- once my keyboard mappings take size of scale into account


scaleTypeConstructor : ScaleType -> (PitchClass -> Scale)
scaleTypeConstructor scaleType =
    if scaleType == ScaleType.major then
        Scale.major

    else
        Scale.minor


nextInList : (a -> Bool) -> List a -> a -> a
nextInList finder list default =
    list
        |> List.Extra.findIndex finder
        |> Maybe.andThen (\index -> List.Extra.getAt (modBy (List.length list) (index + 1)) list)
        |> Maybe.withDefault default


prevInList : (a -> Bool) -> List a -> a -> a
prevInList finder list default =
    list
        |> List.Extra.findIndex finder
        |> Maybe.andThen (\index -> List.Extra.getAt (modBy (index + 1) (List.length list)) list)
        |> Maybe.withDefault default
