module LetterStream exposing (..)

import Time


fadeTime : Int
fadeTime =
    1000


type LetterStream
    = Empty
    | LetterStream Time.Posix Bool ( Char, Time.Posix ) (List ( Char, Time.Posix ))


init : LetterStream
init =
    Empty


isEmpty : LetterStream -> Bool
isEmpty letterStream =
    case letterStream of
        Empty ->
            True

        LetterStream _ _ _ _ ->
            False


toList : LetterStream -> List ( Char, Time.Posix )
toList letterStream =
    case letterStream of
        Empty ->
            []

        LetterStream _ _ c cs ->
            c :: cs


fromList : Bool -> Time.Posix -> List ( Char, Time.Posix ) -> LetterStream
fromList active time list =
    case list of
        [] ->
            Empty

        c :: cs ->
            LetterStream time active c cs


mapToList : (Time.Posix -> ( Char, Time.Posix ) -> a) -> LetterStream -> List a
mapToList fn letterStream =
    case letterStream of
        Empty ->
            []

        LetterStream time _ c cs ->
            List.map (fn time) (c :: cs)


diminish : LetterStream -> LetterStream
diminish letterStream =
    case letterStream of
        Empty ->
            Empty

        LetterStream t _ lastPair pairs ->
            LetterStream t False lastPair pairs


tick : Time.Posix -> LetterStream -> LetterStream
tick time letterStream =
    let
        tooOld ( _, t ) =
            (Time.posixToMillis time - Time.posixToMillis t) < fadeTime
    in
    toList letterStream
        |> List.filter tooOld
        |> fromList (isActive letterStream) time


isActive : LetterStream -> Bool
isActive letterStream =
    case letterStream of
        Empty ->
            False

        LetterStream _ b _ _ ->
            b


append : ( Char, Time.Posix ) -> LetterStream -> LetterStream
append ( char, time ) letterStream =
    case letterStream of
        Empty ->
            LetterStream time True ( char, time ) []

        LetterStream _ _ lastPair pairs ->
            LetterStream time True ( char, time ) (lastPair :: pairs)
