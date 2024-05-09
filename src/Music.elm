module Music exposing (..)

import Music.PitchClass exposing (PitchClass)
import Music.Scale as Scale exposing (Scale)
import Music.ScaleType as ScaleType exposing (ScaleType)


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



-- TODO add more interesting scales
-- Can do pentatonic and blues
-- once my keyboard mappings take size of scale into account


scaleTypeConstructor : ScaleType -> (PitchClass -> Scale)
scaleTypeConstructor scaleType =
    if scaleType == ScaleType.major then
        Scale.major

    else
        Scale.minor
