import * as $dynamic from "../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../gleam_stdlib/gleam/list.mjs";
import * as $attribute from "../lustre/lustre/attribute.mjs";
import * as $element from "../lustre/lustre/element.mjs";
import * as $html from "../lustre/lustre/element/html.mjs";
import * as $event from "../lustre/lustre/event.mjs";
import { Ok, Error, toList } from "./gleam.mjs";
import {
  do_decode_input_file as do_decode_file_url,
  do_drop_file,
  do_click_input,
} from "./htmle-native.mjs";

export function drop_file(url) {
  return do_drop_file(url);
}

export function file_picker(mimes, handler, div_attrs, button_attrs, children) {
  return $html.div(
    div_attrs,
    toList([
      $html.button(
        $list.prepend(
          button_attrs,
          $event.on(
            "click",
            (dyn) => {
              do_click_input(dyn);
              return new Error(toList([]));
            },
          ),
        ),
        children,
      ),
      $html.input(
        toList([
          $attribute.style(toList([["display", "none"]])),
          $attribute.type_("file"),
          $attribute.accept(mimes),
          $event.on(
            "change",
            (dyn) => { return new Ok(handler(do_decode_file_url(dyn))); },
          ),
        ]),
      ),
    ]),
  );
}
