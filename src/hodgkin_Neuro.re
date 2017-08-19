open Reprocessing;

module Util = Hodgkin_Util;

module Color = Hodgkin_Color;

module Fitzhugh = Hodgkin_Neuro_Fitzhugh;

module MorrisLecar = Hodgkin_Neuro_MorrisLecar;

let colorOf value => Utils.lerpColor low::Color.red high::Color.blue ::value;

let draw2d
    ::squareSize=200.
    ::varPulse
    ::lowPulseVal=0.
    ::highPulseVal=1.
    ::varColor
    ::lowColorVal=0.
    ::highColorVal=0.2
    env =>
  env |>
  Util.withContext (
    fun () => {
      env |>
      Util.withContext (
        fun () => {
          Draw.fill Color.darkGray env;
          let scale = 1. +. max 0. (-. varPulse);
          let size = squareSize *. scale;
          Util.drawRectfCenter width::size height::size env
        }
      );
      Draw.fill Color.gray env;
      Util.drawRectfCenter width::squareSize height::squareSize env;
      let neuroColor = colorOf varColor;
      env |>
      Util.withContext (
        fun () => {
          Draw.fill neuroColor env;
          let scale = max 0. varPulse;
          let size =
            squareSize *. 0.1 +.
            squareSize *. Utils.lerpf low::0. high::0.9 value::scale;
          Util.drawRectfCenter width::size height::size env
        }
      )
    }
  );

let drawFitzhugh ::squareSize=200. (state: Fitzhugh.state) env =>
  Fitzhugh.(
    draw2d ::squareSize varPulse::state.v varColor::(state.w *. 5.) env
  );

let drawMorrisLecar ::squareSize=200. (state: MorrisLecar.state) env => {
  Js.log state;
  MorrisLecar.(
    draw2d
      ::squareSize
      varPulse::(Utils.norm low::lowV high::highV value::state.v)
      varColor::(Utils.norm low::lowN high::highN value::state.n)
      env
  )
};
