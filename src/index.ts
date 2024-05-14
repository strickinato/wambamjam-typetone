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
const synth = new Tone.Synth()
const distortion = new Tone.Distortion(0.5)
const filter = new Tone.Filter({ type: 'lowpass', Q: 15 })
const tremolo = new Tone.Tremolo({frequency: '100hz'})
const delay = new Tone.FeedbackDelay({delayTime: 0.2, wet: 0.3})
const reverb = new Tone.Reverb({decay: 1, wet: 0.5})
reverb.generate();
synth.chain(filter, distortion, delay, reverb, Tone.Master)

function normalToFreq(float) {
  const midiScale = Math.floor(float * 90) + 20
  const freq = Tone.Frequency(midiScale, "midi").toFrequency();
  return freq

}

// Port Handlers
//
app.ports.sendXY.subscribe((data) => {

  // tremolo.depth.value = data.y
  // reverb.decay.value = Math.floor(data.x *
  filter.frequency.value = normalToFreq(data.x)
  distortion.distortion = data.y
  delay.delayTime.rampTo(data.z , 0.1)
})

app.ports.sendNote.subscribe((data) => {
  if (data.synthEnabled && data.on) {
    synth.triggerAttackRelease(data.noteFreq, "8n", Tone.context.currentTime);
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
