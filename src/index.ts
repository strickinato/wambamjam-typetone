import * as Tone from "tone";

const node = document.getElementById("mount");
const app = Elm.Main.init({
  node,
});

const synth = new Tone.Synth().toDestination();

app.ports.sendNote.subscribe((note) => {
  synth.triggerAttackRelease(note, "8n");
})
