open Reprocessing;

module Util = Hodgkin_Util;

module Color = Hodgkin_Color;

module Fitzhugh = Hodgkin_Neuro_Fitzhugh;

let colorOfState st =>
  Utils.lerpColor low::Color.red high::Color.blue value::(st.Fitzhugh.w *. 5.);

let draw ::squareSize=200. state env =>
  env |>
  Util.withContext (
    fun () => {
      env |>
      Util.withContext (
        fun () => {
          Draw.fill Color.darkGray env;
          let scale = 1. +. max 0. (-. state.Fitzhugh.v);
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
          let scale = max 0. state.Fitzhugh.v;
          let size = squareSize *. 0.1 +. squareSize *. scale;
          Util.drawRectfCenter width::size height::size env
        }
      )
    }
  );
