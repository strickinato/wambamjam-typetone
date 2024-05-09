import * as Tone from "tone";

const node = document.getElementById("mount");
const flags = {
  windowX: window.innerWidth,
  windowY: window.innerHeight
}

const app = Elm.Main.init({
  node,
  flags,
});

// CONSTANTS
let savedMidiAccess


// Synth setup
//
const synth = new Tone.Synth().toDestination();
const distortion = new Tone.Distortion(0).toDestination();
const filter = new Tone.Filter({ type: 'lowpass', Q: 25 }).toDestination();
const reverb = new Tone.Reverb({decay: 5}).toDestination();
synth.connect(distortion).connect(filter).connect(reverb);

function normalToFreq(float) {
  const midiScale = Math.floor(float * 128)

  return Tone.Frequency(midiScale, "midi").toFrequency();

}

// Port Handlers
//
app.ports.sendXY.subscribe((data) => {
  const cutoff = normalToFreq(data.x)
  filter.frequency.value = cutoff
  filter.Q.value = Math.floor(data.y * 30)
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

app.ports.allNotesOff.subscribe((data) => {
  if (savedMidiAccess !== undefined) {
    const output = savedMidiAccess.outputs.forEach( output => {
      output.send([0xB0, 123, 0])
    })
  }
})
