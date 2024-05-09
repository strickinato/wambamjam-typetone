import * as Tone from "tone";

const node = document.getElementById("mount");
const app = Elm.Main.init({
  node,
});

const synth = new Tone.Synth().toDestination();

let savedMidiAccess

app.ports.allNotesOff.subscribe((data) => {
  console.log("BUTT")
  if (savedMidiAccess !== undefined) {
    const output = savedMidiAccess.outputs.forEach( output => {
      output.send([0xB0, 123, 0])
    })
  }
})

app.ports.sendNote.subscribe((data) => {
  if (data.synthEnabled && data.on) {
    synth.triggerAttackRelease(data.noteFreq, "8n");
  }
  if (savedMidiAccess !== undefined && data.midiId !== undefined) {
    const output = savedMidiAccess.outputs.get(data.midiId)
    if (output) {
      if (data.on) {
        console.log('starting', data.midiNote)
        output.send([0x90, data.midiNote, 0x7f])
      } else {
        console.log('ending', data.midiNote)
        output.send([0x83, data.midiNote, 0x7f])
      }
    }
  }
})

app.ports.startMidi.subscribe((data) => {
  const onMIDISuccess = (midiAccess) => {
    savedMidiAccess = midiAccess

    const outputs = new Array()

    midiAccess.outputs.forEach((output) => {
      outputs.push({ id: output.id, name: output.name })
    })

    app.ports.midiReceivedInfo.send({success: true, outputs})
  }
  const onMIDIFailure = () => {
    app.ports.midiReceivedInfo.send({success: false, message: "access denied"})
  }

  if (navigator.requestMIDIAccess) {
      navigator.requestMIDIAccess().then(onMIDISuccess, onMIDIFailure);
  } else {
    console.log("Midid not supported")
  }
})
