import * as Tone from "tone";

const node = document.getElementById("mount");
const app = Elm.Main.init({
  node,
});

const synth = new Tone.Synth().toDestination();

let savedMidiAccess

app.ports.sendNote.subscribe((data) => {
  synth.triggerAttackRelease(data.noteString, "8n");
  if (savedMidiAccess !== undefined) {
    savedMidiAccess.outputs.get('1141572896').send([0x90, 60, 0x7f])
  }
})

app.ports.startMidi.subscribe((data) => {
  const onMIDISuccess = (midiAccess) => {
    const inputs = midiAccess.inputs;
    const outputs = midiAccess.outputs;
    savedMidiAccess = midiAccess
    console.log(Object.keys(inputs))
    outputs.forEach((output) => {
      console.log(output)
    })
  }
  const onMIDIFailure = () => {
    console.log("MIDI BROKEN")
  }

  if (navigator.requestMIDIAccess) {
      navigator.requestMIDIAccess().then(onMIDISuccess, onMIDIFailure);
  } else {
    console.log("Midid not supported")
  }
})
