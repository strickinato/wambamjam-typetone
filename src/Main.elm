port module Main exposing (..)

import Browser
import Browser.Events
import Css exposing (..)
import Html.Styled as Html exposing (Attribute, Html)
import Html.Styled.Attributes as Attributes exposing (css)
import Html.Styled.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Keyboard.Event
import Keyboard.Key
import LetterStream exposing (LetterStream)
import Midi
import Music
import Music.Pitch
import Music.PitchClass
import Music.Scale
import Music.ScaleType
import Task
import Time
import XYZControl exposing (XYZControl)


port sendNote : Encode.Value -> Cmd msg


port allNotesOff : () -> Cmd msg


port startMidi : () -> Cmd msg


port sendXY : Encode.Value -> Cmd msg


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
    , currentPressedLetter : Maybe Char
    , letterStream : LetterStream
    , midiState : MidiState
    , internalSynth : Bool
    , xyzControl : XYZControl
    , windowSize : ( Int, Int )
    , displaySetting : DisplaySetting
    }


type Msg
    = NoOp
    | DisplaySettingNext
    | KeyPressDown Char Music.Pitch.Pitch
    | KeyPressUp Char Music.Pitch.Pitch
    | KeyAddToLetterStream Char Time.Posix
    | TickLetterStream Time.Posix
    | SynthToggle Bool
    | MidiStart
    | MidiSelectPort Midi.Port
    | MidiReceivedInfo Encode.Value
    | MidiPanic
    | ChangeScaleType Music.ScaleType.ScaleType
    | XYControlMove ( Float, Float, Float )
    | WindowSizeChange ( Int, Int )


type alias MusicMode =
    { pitchClass : Music.PitchClass.PitchClass
    , scaleType : Music.ScaleType.ScaleType
    }


type MidiState
    = NoConnection
    | Started
    | ConnectionMade (List Midi.Port) (Maybe Midi.Port) Bool
    | ConnectionFailed String


type alias Flags =
    { windowX : Int
    , windowY : Int
    }


type DisplaySetting
    = All
    | Minimal
    | None


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { currentMusicMode = { pitchClass = Music.PitchClass.c, scaleType = Music.ScaleType.major }
      , currentPressedLetter = Nothing
      , letterStream = LetterStream.init
      , internalSynth = True
      , midiState = NoConnection
      , xyzControl = { x = 0.5, y = 0.5, z = 0.5 }
      , windowSize = ( flags.windowX, flags.windowY )
      , displaySetting = All
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DisplaySettingNext ->
            let
                next =
                    case model.displaySetting of
                        All ->
                            Minimal

                        Minimal ->
                            None

                        None ->
                            All
            in
            ( { model | displaySetting = next }, Cmd.none )

        KeyPressDown char pitch ->
            ( model
            , Cmd.batch
                [ sendNote
                    (encodeNote
                        { midiId = getCurrentMidiId model
                        , synthEnabled = model.internalSynth
                        , pitch = pitch
                        , on = True
                        }
                    )
                , Task.perform (KeyAddToLetterStream char) Time.now
                ]
            )

        KeyPressUp char pitch ->
            ( { model | letterStream = LetterStream.diminish model.letterStream }
            , Cmd.batch
                [ sendNote
                    (encodeNote
                        { midiId = getCurrentMidiId model
                        , synthEnabled = model.internalSynth
                        , pitch = pitch
                        , on = False
                        }
                    )
                ]
            )

        KeyAddToLetterStream char posix ->
            ( { model | letterStream = LetterStream.append ( char, posix ) model.letterStream }
            , Cmd.none
            )

        TickLetterStream posix ->
            ( { model | letterStream = LetterStream.tick posix model.letterStream }
            , Cmd.none
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

        XYControlMove ( x, y, z ) ->
            let
                { xyzControl } =
                    model

                encoded =
                    Encode.object
                        [ ( "x", Encode.float x )
                        , ( "y", Encode.float y )
                        , ( "z", Encode.float z )
                        ]
            in
            ( { model | xyzControl = { xyzControl | x = x, y = y, z = z } }, sendXY encoded )

        WindowSizeChange size ->
            ( { model | windowSize = size }, Cmd.none )

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
    let
        displayWithSetting important html =
            case ( model.displaySetting, important ) of
                ( All, _ ) ->
                    html

                ( Minimal, True ) ->
                    html

                ( Minimal, False ) ->
                    Html.text ""

                ( None, _ ) ->
                    Html.text ""
    in
    Html.div
        [ css
            [ displayFlex
            , flexDirection column
            , justifyContent spaceBetween
            , height (pct 100)
            , backgroundColor colors.black
            , color colors.green
            , fontFamily monospace
            , paddingTop (px 24)
            ]
        , Events.preventDefaultOn "wheel" (handleWheel model |> Decode.map (\d -> ( d, True )))
        ]
        [ viewLetterStream model.displaySetting model.letterStream
        , Html.div [ css [ displayFlex, flexDirection column, gap ] ]
            [ displayWithSetting False <| viewMusicMode model.currentMusicMode
            , displayWithSetting False <| viewInternalSynth model.internalSynth
            , displayWithSetting False <| viewMidiControl model.midiState
            , displayWithSetting True <| viewXYControl model.displaySetting model.xyzControl
            ]
        ]


viewXYControl : DisplaySetting -> XYZControl -> Html Msg
viewXYControl displaySetting xy =
    let
        visualization =
            Html.div [ css [ displayFlex, flexDirection column, gap ] ]
                [ Html.div
                    [ css
                        [ border3 (px 1) solid colors.green
                        , width (px 100)
                        , height (px 100)
                        , position relative
                        ]
                    ]
                    [ Html.div
                        [ css
                            [ position absolute
                            , width (px 5)
                            , height (px 5)
                            , backgroundColor colors.green
                            , left (px <| xy.x * 95)
                            , bottom (px <| xy.y * 95)
                            ]
                        ]
                        []
                    ]
                , Html.div
                    [ css
                        [ position relative
                        , width (px 100)
                        , borderBottom3 (px 1) solid colors.green
                        ]
                    ]
                    [ Html.div
                        [ css
                            [ position absolute
                            , width (px 5)
                            , height (px 5)
                            , backgroundColor colors.green
                            , left (px <| xy.z * 95)
                            , top (px -2.5)
                            ]
                        ]
                        []
                    ]
                ]
    in
    case displaySetting of
        None ->
            Html.text ""

        Minimal ->
            Html.div [ css [ paddingLeft (px 8) ] ] [ visualization ]

        All ->
            section "XYZ" <|
                Html.div [ css [ paddingTop (px 8), displayFlex, gap ] ]
                    [ visualization
                    , Html.div []
                        [ Html.p [] [ Html.text "x: filter" ]
                        , Html.p [] [ Html.text "y: distortion" ]
                        , Html.p [] [ Html.text "z: delay time" ]
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


viewLetterStream : DisplaySetting -> LetterStream -> Html Msg
viewLetterStream displaySetting letterStream =
    let
        toFadedLetter time ( char, setTime ) =
            let
                normal =
                    1 - toFloat (Time.posixToMillis time - Time.posixToMillis setTime) / 1000
            in
            Html.span [ css [ opacity (num normal) ] ] [ Html.text (String.fromChar char) ]

        internal =
            Html.div [ css [ minHeight (px 100), fontSize (px 72), textAlign right ] ]
                (List.reverse <| LetterStream.mapToList toFadedLetter letterStream)
    in
    case displaySetting of
        All ->
            section "LETTER" internal

        Minimal ->
            Html.div [ css [ width (px 400) ] ] [ internal ]

        None ->
            Html.div [ css [ width (px 400) ] ] [ internal ]


section : String -> Html Msg -> Html Msg
section title internal =
    Html.div
        [ css
            [ border3 (px 1) solid colors.black
            , width (px 400)
            , paddingLeft (px 24)
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
        , Browser.Events.onResize (\x y -> WindowSizeChange ( x, y ))
        , midiReceivedInfo MidiReceivedInfo
        , Browser.Events.onMouseMove (handleMouse model)
        , if (not << LetterStream.isEmpty) model.letterStream then
            Browser.Events.onAnimationFrame TickLetterStream

          else
            Sub.none
        ]


handleWheel : Model -> Decoder Msg
handleWheel model =
    Decode.field "deltaY" Decode.float
        |> Decode.map
            (\deltaY ->
                let
                    { x, y, z } =
                        model.xyzControl

                    newZ =
                        ((deltaY / 1000) + model.xyzControl.z)
                            |> clamp 0 1
                in
                XYControlMove ( x, y, newZ )
            )


handleMouse : Model -> Decoder Msg
handleMouse model =
    Decode.map2
        (\x y ->
            XYControlMove
                ( x / toFloat (Tuple.first model.windowSize)
                , 1 - (y / toFloat (Tuple.second model.windowSize))
                , model.xyzControl.z
                )
        )
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)


handleDown : Model -> Keyboard.Event.KeyboardEvent -> Maybe Msg
handleDown model event =
    case Debug.log "key" event.key of
        Just "Escape" ->
            Just MidiPanic

        Just "}" ->
            Just (ChangeScaleType (Music.nextScaleType model.currentMusicMode.scaleType))

        Just "?" ->
            Just DisplaySettingNext

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
                    |> Maybe.map (KeyPressUp keyChar)

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
