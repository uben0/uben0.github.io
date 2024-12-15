import Essentia from '../../essentia/essentia.js-core.es.js';
import { EssentiaWASM } from '../../essentia/essentia-wasm.es.js';
import { toList } from '../prelude.mjs';

const essentia = new Essentia(EssentiaWASM);
var audioContext = null;
const worker = new Worker(new URL("./audio-native-worker.mjs", import.meta.url), {type: "module"});

var callback = [];

worker.onmessage = function(event) {
  callback.pop()(toList(event.data));
}

export function do_beat_track(url, dispatch) {
  if (audioContext == null) {
    audioContext = new AudioContext();
  }
  callback.push(dispatch);
  essentia.getAudioBufferFromURL(url, audioContext).then(audio => {
    const signal = essentia.audioBufferToMonoSignal(audio);
    worker.postMessage({sampleRate: audio.sampleRate, channelData: signal});
  });
}
