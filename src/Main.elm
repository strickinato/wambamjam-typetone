port module Main exposing (..)

import Browser
import Browser.Events
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Json.Encode as Encode
import Keyboard.Event
import Keyboard.Key
import Midi
import Music
import Music.Pitch
import Music.PitchClass
import Music.Scale
import Music.ScaleType


port sendNote : Encode.Value -> Cmd msg


port allNotesOff : () -> Cmd msg


port startMidi : () -> Cmd msg


port midiReceivedInfo : (Encode.Value -> msg) -> Sub msg


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
    { currentMusicMode : MusicMode
    , currentPressedLetter : Maybe String
    , midiState : MidiState
    , internalSynth : Bool
    }


type Msg
    = NoOp
    | KeyPressDown Char Music.Pitch.Pitch
    | KeyPressUp Music.Pitch.Pitch
    | SynthToggle Bool
    | MidiStart
    | MidiSelectPort Midi.Port
    | MidiReceivedInfo Encode.Value
    | MidiPanic
    | ChangeScaleType Music.ScaleType.ScaleType


type alias MusicMode =
    { pitchClass : Music.PitchClass.PitchClass
    , scaleType : Music.ScaleType.ScaleType
    }


type MidiState
    = NoConnection
    | Started
    | ConnectionMade (List Midi.Port) (Maybe Midi.Port) Bool
    | ConnectionFailed String


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentMusicMode = { pitchClass = Music.PitchClass.c, scaleType = Music.ScaleType.major }
      , currentPressedLetter = Nothing
      , internalSynth = True
      , midiState = NoConnection
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressDown char pitch ->
            ( { model | currentPressedLetter = char |> String.fromChar |> Just }
            , sendNote
                (encodeNote
                    { midiId = getCurrentMidiId model
                    , synthEnabled = model.internalSynth
                    , pitch = pitch
                    , on = True
                    }
                )
            )

        KeyPressUp pitch ->
            ( { model | currentPressedLetter = Nothing }
            , sendNote
                (encodeNote
                    { midiId = getCurrentMidiId model
                    , synthEnabled = model.internalSynth
                    , pitch = pitch
                    , on = False
                    }
                )
            )

        SynthToggle bool ->
            ( { model | internalSynth = bool }, Cmd.none )

        MidiStart ->
            ( { model | midiState = Started }, startMidi () )

        MidiSelectPort port_ ->
            case model.midiState of
                ConnectionMade info _ enabled ->
                    ( { model | midiState = ConnectionMade info (Just port_) enabled }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MidiReceivedInfo value ->
            case Decode.decodeValue Midi.decodeInfo value of
                Ok (Midi.Success info) ->
                    ( { model
                        | midiState = ConnectionMade info (List.head info) True
                        , internalSynth = False
                      }
                    , Cmd.none
                    )

                Ok (Midi.Failed error) ->
                    ( { model | midiState = ConnectionFailed error }, Cmd.none )

                Err err ->
                    ( { model | midiState = ConnectionFailed (Decode.errorToString err) }, Cmd.none )

        MidiPanic ->
            ( { model | currentPressedLetter = Nothing }
            , allNotesOff ()
            )

        ChangeScaleType scaleType ->
            let
                oldMode =
                    model.currentMusicMode
            in
            ( { model | currentMusicMode = { oldMode | scaleType = scaleType } }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


getCurrentMidiId : Model -> Maybe String
getCurrentMidiId { midiState } =
    case midiState of
        ConnectionMade _ (Just { id }) True ->
            Just id

        _ ->
            Nothing


getCurrentScale : Model -> Music.Scale.Scale
getCurrentScale { currentMusicMode } =
    Music.scaleTypeConstructor currentMusicMode.scaleType Music.PitchClass.c


view : Model -> Html Msg
view model =
    Html.div
        [ css
            [ displayFlex
            , flexDirection column
            , justifyContent spaceBetween
            , height (pct 100)
            , backgroundColor colors.black
            , color colors.green
            , fontFamily monospace
            ]
        ]
        [ viewLetter model.currentPressedLetter
        , Html.div [ css [ displayFlex, flexDirection column, gap ] ]
            [ viewMusicMode model.currentMusicMode
            , viewInternalSynth model.internalSynth
            , viewMidiControl model.midiState
            ]
        ]


viewMusicMode : MusicMode -> Html Msg
viewMusicMode { scaleType, pitchClass } =
    section "MUSIC" <|
        Html.div []
            [ Html.text
                (Music.PitchClass.toString pitchClass
                    ++ " "
                    ++ Music.scaleTypeToString scaleType
                )
            ]


viewInternalSynth : Bool -> Html Msg
viewInternalSynth enabled =
    section "SYNTH" <|
        Html.div []
            [ Html.input
                [ Attributes.type_ "checkbox"
                , Attributes.checked enabled
                , Events.onCheck SynthToggle
                ]
                []
            ]


viewMidiControl : MidiState -> Html Msg
viewMidiControl midiState =
    section "MIDI" <|
        let
            viewPort maybeSelected port_ =
                let
                    portDisplayName =
                        if maybeSelected == Just port_ then
                            "**" ++ port_.name

                        else
                            port_.name
                in
                Html.div
                    [ css [ cursor pointer ]
                    , Attributes.tabindex 0
                    , Events.onClick (MidiSelectPort port_)
                    ]
                    [ Html.text portDisplayName ]
        in
        case midiState of
            NoConnection ->
                Html.button [ Events.onClick MidiStart ] [ Html.text "Start Midi" ]

            Started ->
                Html.text "waiting"

            ConnectionMade info maybeSelectedPort enabled ->
                Html.div [] (List.map (viewPort maybeSelectedPort) info)

            ConnectionFailed str ->
                Html.text ("Failed:" ++ str)


viewLetter : Maybe String -> Html Msg
viewLetter maybeLetter =
    let
        internal =
            Html.div [ css [ minHeight (px 100), fontSize (px 72) ] ]
                [ maybeView maybeLetter
                    Html.text
                ]
    in
    section "LETTER" internal


section : String -> Html Msg -> Html Msg
section title internal =
    Html.div
        [ css
            [ border3 (px 1) solid colors.black
            , width (px 400)
            , paddingLeft (px 8)
            ]
        ]
        [ Html.div [ css [ borderBottom3 (px 1) solid colors.green ] ] [ Html.text title ]
        , internal
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyDown (Keyboard.Event.considerKeyboardEvent (handleDown model))
        , Browser.Events.onKeyUp (Keyboard.Event.considerKeyboardEvent (handleUp model))
        , midiReceivedInfo MidiReceivedInfo
        ]


handleDown : Model -> Keyboard.Event.KeyboardEvent -> Maybe Msg
handleDown model event =
    case event.key of
        Just "Escape" ->
            Just MidiPanic

        Just "}" ->
            Just (ChangeScaleType (Music.nextScaleType model.currentMusicMode.scaleType))

        Just keyStr ->
            toChar keyStr
                |> Maybe.andThen
                    (\char ->
                        if Char.isAlpha char || char == ' ' then
                            event
                                |> keyboardToNote model
                                |> Maybe.map (KeyPressDown char)

                        else
                            Nothing
                    )

        Nothing ->
            Nothing


handleUp : Model -> Keyboard.Event.KeyboardEvent -> Maybe Msg
handleUp model event =
    case event.key |> Maybe.andThen toChar of
        Just keyChar ->
            if Char.isAlpha keyChar || keyChar == ' ' then
                event
                    |> keyboardToNote model
                    |> Maybe.map KeyPressUp

            else
                Nothing

        Nothing ->
            Nothing


keyboardToNote : Model -> Keyboard.Event.KeyboardEvent -> Maybe Music.Pitch.Pitch
keyboardToNote model event =
    event.key
        |> Maybe.andThen frequencyMap
        |> Maybe.map
            (\degree_ ->
                getCurrentScale model
                    |> Music.Scale.degree degree_
                    |> Music.Pitch.fromPitchClassInOctave 4
            )


toChar : String -> Maybe Char
toChar string =
    case string |> String.toList of
        [ x ] ->
            Just x

        _ ->
            Nothing


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


encodeNote : { midiId : Maybe String, synthEnabled : Bool, pitch : Music.Pitch.Pitch, on : Bool } -> Encode.Value
encodeNote { synthEnabled, midiId, pitch, on } =
    Encode.object
        [ ( "noteFreq", Encode.float (Music.Pitch.toFrequency pitch) )
        , ( "midiNote", Encode.int (Music.Pitch.toMIDINoteNumber pitch) )
        , ( "synthEnabled", Encode.bool synthEnabled )
        , ( "on", Encode.bool on )
        , case midiId of
            Just midiId_ ->
                ( "midiId", Encode.string midiId_ )

            Nothing ->
                ( "", Encode.null )
        ]


maybeView : Maybe a -> (a -> Html msg) -> Html msg
maybeView maybe view_ =
    case maybe of
        Just a ->
            view_ a

        Nothing ->
            Html.text ""


colors =
    { green = rgb 0 255 0, black = rgb 0 0 0 }


gap =
    property "gap" "8px"
