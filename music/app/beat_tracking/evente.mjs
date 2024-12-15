import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import * as $event from "../lustre/lustre/event.mjs";
import { do_decode_audio_time as do_decode_current_time } from "./evente-native.mjs";
import { Ok } from "./gleam.mjs";
import * as $time from "./time.mjs";

export function on_play(message) {
  return $event.on(
    "play",
    (dyn) => {
      return new Ok(message($time.during(do_decode_current_time(dyn))));
    },
  );
}

export function on_pause(message) {
  return $event.on(
    "pause",
    (dyn) => {
      return new Ok(message($time.during(do_decode_current_time(dyn))));
    },
  );
}
