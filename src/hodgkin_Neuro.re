open Reprocessing;

module Util = Hodgkin_Util;

module Color = Hodgkin_Color;

type state = {
  v: float,
  w: float
};

let initState = {v: 0.5, w: 0.5};

let step ::a=(-0.1) ::b=0.01 ::c=0.02 ::input=0. ::tau=1. st => {
  let {v, w} = st;
  let dv = v *. (a -. v) *. (v -. 1.) -. w +. input;
  let dw = b *. v -. c *. w;
  {v: v +. dv *. tau, w: w +. dw *. tau}
};

let colorOfState st =>
  Utils.lerpColor low::Color.red high::Color.blue value::(st.w *. 5.);

let draw ::squareSize=200. state env =>
  env |>
  Util.withContext (
    fun () => {
      env |>
      Util.withContext (
        fun () => {
          Draw.fill Color.darkGray env;
          let scale = 1. +. max 0. (-. state.v);
          let size = squareSize *. scale;
          Util.drawRectfCenter width::size height::size env
        }
      );
      Draw.fill Color.gray env;
      Util.drawRectfCenter width::squareSize height::squareSize env;
      let neuroColor = colorOfState state;
      env |>
      Util.withContext (
        fun () => {
          Draw.fill neuroColor env;
          let scale = max 0. state.v;
          let size = squareSize *. 0.1 +. squareSize *. scale;
          Util.drawRectfCenter width::size height::size env
        }
      )
    }
  );
