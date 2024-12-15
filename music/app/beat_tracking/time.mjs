import * as $float from "../gleam_stdlib/gleam/float.mjs";
import * as $int from "../gleam_stdlib/gleam/int.mjs";
import * as $order from "../gleam_stdlib/gleam/order.mjs";
import * as $effect from "../lustre/lustre/effect.mjs";
import { CustomType as $CustomType, divideFloat } from "./gleam.mjs";
import { do_cancel, do_now, do_delay } from "./time-native.mjs";

export class Time extends $CustomType {
  constructor(ms) {
    super();
    this.ms = ms;
  }
}

export class Duration extends $CustomType {
  constructor(ms) {
    super();
    this.ms = ms;
  }
}

class Period extends $CustomType {
  constructor(epoch) {
    super();
    this.epoch = epoch;
  }
}

export function period(epoch) {
  return new Period(epoch);
}

export function relative(period, at) {
  return new Time(at.ms - period.epoch.ms);
}

export function absolute(period, at) {
  return new Time(period.epoch.ms + at.ms);
}

export function elapsed(at) {
  return new Duration(at.ms);
}

export function diff(lhs, rhs) {
  return new Duration(lhs.ms - rhs.ms);
}

export function advance(instant, duration) {
  return new Time(instant.ms + duration.ms);
}

export function rewind(instant, duration) {
  return new Time(instant.ms - duration.ms);
}

export function add(lhs, rhs) {
  return new Duration(lhs.ms + rhs.ms);
}

export function sub(lhs, rhs) {
  return new Duration(lhs.ms - rhs.ms);
}

export function scale(duration, factor) {
  return new Duration($float.round($int.to_float(duration.ms) * factor));
}

export function during(seconds) {
  return new Duration($float.round(seconds * 1000.0));
}

export function zero() {
  return new Time(0);
}

export function compare(lhs, rhs) {
  return $int.compare(lhs.ms, rhs.ms);
}

export function seconds(duration) {
  return divideFloat($int.to_float(duration.ms), 1000.0);
}

export function cancel() {
  return do_cancel();
}

export function now() {
  return new Time(do_now());
}

export function delay(message, duration) {
  return $effect.from(
    (dispatch) => {
      return do_delay(() => { return dispatch(message); }, duration.ms);
    },
  );
}

export function schedule(message, instant) {
  let ms = diff(instant, now()).ms;
  return $effect.from(
    (dispatch) => {
      let $ = ms <= 0;
      if ($) {
        return dispatch(message);
      } else {
        return do_delay(() => { return dispatch(message); }, ms);
      }
    },
  );
}
