port module Main exposing (..)

import Browser
import Browser.Events
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard.Event
import Music.Pitch
import Music.PitchClass
import Music.Scale


port sendNote : Encode.Value -> Cmd msg


port startMidi : () -> Cmd msg


main =
    Browser.document
        { init = init
        , update = update
        , view =
            \model ->
                { body = [ view model |> Html.toUnstyled ]
                , title = "TYPE MUSIC"
                }
        , subscriptions = subscriptions
        }


type alias Model =
    { currentScale : Music.Scale.Scale
    , currentPressedLetter : Maybe String
    , midiState : MidiState
    }


type Msg
    = NoOp
    | HandleKeyPress Keyboard.Event.KeyboardEvent
    | ReleaseKey
    | StartMidi


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentScale = Music.Scale.major Music.PitchClass.c
      , currentPressedLetter = Nothing
      , midiState = NoConnection
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleKeyPress keyboardEvent ->
            let
                degree =
                    keyboardEvent.key
                        |> Maybe.andThen frequencyMap
            in
            case degree of
                Just degree_ ->
                    let
                        note =
                            model.currentScale
                                |> Music.Scale.degree degree_
                                |> Music.Pitch.fromPitchClassInOctave 4
                    in
                    ( { model | currentPressedLetter = keyboardEvent.key }
                    , sendNote (encodeNote note)
                    )

                Nothing ->
                    ( model, Cmd.none )

        ReleaseKey ->
            ( { model | currentPressedLetter = Nothing }, Cmd.none )

        StartMidi ->
            ( { model | midiState = Started }, startMidi () )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ viewLetter |> maybeView model.currentPressedLetter
        , viewMidiControl model.midiState
        ]


viewMidiControl : MidiState -> Html Msg
viewMidiControl midiState =
    case midiState of
        NoConnection ->
            Html.button [ Events.onClick StartMidi ] [ Html.text "Start Midi" ]

        Started ->
            Html.text "waiting"

        Failed ->
            Html.text "failed"


type MidiState
    = NoConnection
    | Started
    | Failed


viewLetter : String -> Html Msg
viewLetter str =
    Html.div [] [ Html.text str ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Decode.map HandleKeyPress Keyboard.Event.decodeKeyboardEvent)
        , Browser.Events.onKeyUp (Decode.succeed ReleaseKey)
        ]


{-| letter to degree
-}
frequencyMap : String -> Maybe Int
frequencyMap char =
    case String.toUpper char of
        " " ->
            Just 1

        "E" ->
            Just 1

        "A" ->
            Just 1

        "R" ->
            Just 1

        "I" ->
            Just 3

        "O" ->
            Just 3

        "T" ->
            Just 3

        "N" ->
            Just 3

        "S" ->
            Just 5

        "L" ->
            Just 5

        "C" ->
            Just 5

        "U" ->
            Just 5

        "D" ->
            Just 5

        "P" ->
            Just 2

        "M" ->
            Just 2

        "H" ->
            Just 2

        "G" ->
            Just 4

        "B" ->
            Just 4

        "F" ->
            Just 4

        "Y" ->
            Just 4

        "W" ->
            Just 6

        "K" ->
            Just 6

        "V" ->
            Just 6

        "X" ->
            Just 6

        "Z" ->
            Just 7

        "J" ->
            Just 7

        "Q" ->
            Just 7

        _ ->
            Nothing


encodeNote : Music.Pitch.Pitch -> Encode.Value
encodeNote pitch =
    Encode.object
        [ ( "noteString", Encode.string (Music.Pitch.toString pitch) )
        , ( "midiNote", Encode.int (Music.Pitch.toMIDINoteNumber pitch) )
        ]


maybeView : Maybe a -> (a -> Html msg) -> Html msg
maybeView maybe view_ =
    case maybe of
        Just a ->
            view_ a

        Nothing ->
            Html.text ""
