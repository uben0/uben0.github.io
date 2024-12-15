import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import { do_beat_track } from "./audio-native.mjs";
import * as $time from "./time.mjs";

export function beat_track(audio_url, message) {
  return $effect.from(
    (dispatch) => {
      return do_beat_track(
        audio_url,
        (beats) => {
          let _pipe = beats;
          let _pipe$1 = $list.map(_pipe, $time.during);
          let _pipe$2 = $list.map(
            _pipe$1,
            (_capture) => { return $time.advance($time.zero(), _capture); },
          );
          let _pipe$3 = message(_pipe$2);
          return dispatch(_pipe$3);
        },
      );
    },
  );
}
