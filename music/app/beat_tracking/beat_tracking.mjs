import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $order from "../gleam_stdlib/gleam/order.mjs";
import { Eq, Gt, Lt } from "../gleam_stdlib/gleam/order.mjs";
import * as $result from "../gleam_stdlib/gleam/result.mjs";
import * as $string from "../gleam_stdlib/gleam/string.mjs";
import * as $lustre from "../lustre/lustre.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import * as $event from "../lustre/lustre/event.mjs";
import * as $audio from "./audio.mjs";
import * as $evente from "./evente.mjs";
import { toList, CustomType as $CustomType, makeError } from "./gleam.mjs";
import * as $htmle from "./htmle.mjs";
import * as $time from "./time.mjs";
import { Duration } from "./time.mjs";

class Playing extends $CustomType {
  constructor(period, is_blinking) {
    super();
    this.period = period;
    this.is_blinking = is_blinking;
  }
}

class Paused extends $CustomType {}

export class Tracking extends $CustomType {}

export class TrackingDone extends $CustomType {
  constructor(beats) {
    super();
    this.beats = beats;
  }
}

class ModelInitial extends $CustomType {}

class Model extends $CustomType {
  constructor(audio_url, play_state, track_state) {
    super();
    this.audio_url = audio_url;
    this.play_state = play_state;
    this.track_state = track_state;
  }
}

export class DoubleBeat extends $CustomType {}

export class Tracked extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class AudioSelected extends $CustomType {
  constructor(url) {
    super();
    this.url = url;
  }
}

export class BlinkStart extends $CustomType {}

export class BlinkEnd extends $CustomType {}

export class Play extends $CustomType {
  constructor(x0) {
    super();
    this[0] = x0;
  }
}

export class Pause extends $CustomType {}

function init(_) {
  return [new ModelInitial(), $effect.none()];
}

function double_beat(beats) {
  return $list.flat_map(
    $list.window_by_2(beats),
    (_use0) => {
      let l = _use0[0];
      let r = _use0[1];
      let diff = (() => {
        let _pipe = $time.diff(r, l);
        return $time.scale(_pipe, 0.5);
      })();
      return toList([
        l,
        (() => {
          let _pipe = l;
          return $time.advance(_pipe, diff);
        })(),
      ]);
    },
  );
}

function next_beat(beats, at) {
  return $list.find(
    beats,
    (beat) => {
      let $ = $time.compare(beat, at);
      if ($ instanceof Gt) {
        return true;
      } else if ($ instanceof Eq) {
        return false;
      } else {
        return false;
      }
    },
  );
}

function next_beat_time(beats, period, now) {
  let _pipe = beats;
  let _pipe$1 = next_beat(_pipe, $time.relative(period, now));
  return $result.map(
    _pipe$1,
    (_capture) => { return $time.absolute(period, _capture); },
  );
}

function blink(model) {
  if (model instanceof Model &&
  model.play_state instanceof Playing &&
  model.track_state instanceof TrackingDone) {
    let period = model.play_state.period;
    let beats = model.track_state.beats;
    let $ = next_beat_time(beats, period, $time.now());
    if ($.isOk()) {
      let beat = $[0];
      return $time.schedule(new BlinkStart(), beat);
    } else {
      return $effect.none();
    }
  } else {
    return $effect.none();
  }
}

function update(model, message) {
  if (message instanceof AudioSelected) {
    let url = message.url;
    if (model instanceof ModelInitial) {
      undefined
    } else {
      let url$1 = model.audio_url;
      $htmle.drop_file(url$1)
    }
    $time.cancel();
    return [
      new Model(url, new Paused(), new Tracking()),
      $audio.beat_track(url, (var0) => { return new Tracked(var0); }),
    ];
  } else if (message instanceof Tracked && model instanceof Model) {
    let beats = message[0];
    return [
      model.withFields({ track_state: new TrackingDone(beats) }),
      blink(model),
    ];
  } else if (message instanceof BlinkStart &&
  model instanceof Model &&
  model.play_state instanceof Playing &&
  !model.play_state.is_blinking) {
    let play_state = model.play_state;
    return [
      model.withFields({
        play_state: play_state.withFields({ is_blinking: true })
      }),
      $time.delay(new BlinkEnd(), $time.during(0.1)),
    ];
  } else if (message instanceof BlinkEnd &&
  model instanceof Model &&
  model.play_state instanceof Playing &&
  model.play_state.is_blinking) {
    let period = model.play_state.period;
    return [
      model.withFields({ play_state: new Playing(period, false) }),
      blink(model),
    ];
  } else if (message instanceof DoubleBeat &&
  model instanceof Model &&
  model.track_state instanceof TrackingDone) {
    let beats = model.track_state.beats;
    return [
      model.withFields({ track_state: new TrackingDone(double_beat(beats)) }),
      blink(model),
    ];
  } else if (message instanceof Play && model instanceof Model) {
    let audio_time = message[0];
    let now = $time.now();
    let period = $time.period($time.rewind(now, audio_time));
    let play_state = new Playing(period, false);
    let model$1 = model.withFields({ play_state: play_state });
    return [model$1, blink(model$1)];
  } else if (message instanceof Pause && model instanceof Model) {
    $time.cancel();
    return [model.withFields({ play_state: new Paused() }), $effect.none()];
  } else {
    return [model, $effect.none()];
  }
}

function view(model) {
  return $html.div(
    toList([$attribute.class$("main")]),
    toList([
      $html.div(
        toList([
          $attribute.class$("centered-content"),
          $attribute.class$(
            (() => {
              if (model instanceof Model &&
              model.play_state instanceof Playing &&
              model.play_state.is_blinking) {
                return "blink-on";
              } else {
                return "blink-off";
              }
            })(),
          ),
        ]),
        toList([
          (() => {
            if (model instanceof Model && model.track_state instanceof Tracking) {
              return $html.div(
                toList([$attribute.class$("loader")]),
                toList([]),
              );
            } else {
              return $element.none();
            }
          })(),
        ]),
      ),
      $html.audio(
        toList([
          $attribute.controls(true),
          $evente.on_play((var0) => { return new Play(var0); }),
          $evente.on_pause((_) => { return new Pause(); }),
          (() => {
            if (model instanceof ModelInitial) {
              return $attribute.none();
            } else {
              let url = model.audio_url;
              return $attribute.src(url);
            }
          })(),
        ]),
        toList([]),
      ),
      $htmle.file_picker(
        toList(["audio/*"]),
        (var0) => { return new AudioSelected(var0); },
        toList([$attribute.class$("file-picker")]),
        toList([
          $attribute.disabled(
            (() => {
              if (model instanceof Model &&
              model.track_state instanceof Tracking) {
                return true;
              } else {
                return false;
              }
            })(),
          ),
        ]),
        toList([$element.text("select audio file")]),
      ),
      $html.button(
        toList([
          $event.on_click(new DoubleBeat()),
          $attribute.disabled(
            (() => {
              if (model instanceof Model &&
              model.track_state instanceof TrackingDone) {
                return false;
              } else {
                return true;
              }
            })(),
          ),
        ]),
        toList([$element.text("double beat")]),
      ),
      (() => {
        if (model instanceof Model && model.track_state instanceof TrackingDone) {
          let beats = model.track_state.beats;
          return $html.pre(
            toList([]),
            toList([
              $html.code(
                toList([]),
                toList([
                  $element.text(
                    $string.inspect(
                      (() => {
                        let _pipe = beats;
                        let _pipe$1 = $list.map(_pipe, $time.elapsed);
                        return $list.map(_pipe$1, $time.seconds);
                      })(),
                    ),
                  ),
                ]),
              ),
            ]),
          );
        } else {
          return $element.none();
        }
      })(),
    ]),
  );
}

export function main() {
  let app = $lustre.application(init, update, view);
  let $ = $lustre.start(app, "#app", undefined);
  if (!$.isOk()) {
    throw makeError(
      "let_assert",
      "beat_tracking",
      224,
      "main",
      "Pattern match failed, no pattern matched the value.",
      { value: $ }
    )
  }
  return $;
}
