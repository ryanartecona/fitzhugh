open Reprocessing;

module Util = Hodgkin_Util;

module Color = Hodgkin_Color;

module Fitzhugh = Hodgkin_Neuro_Fitzhugh;

module MorrisLecar = Hodgkin_Neuro_MorrisLecar;

let colorOf value => Utils.lerpColor low::Color.red high::Color.blue ::value;

let draw2d ::squareSize=200. ::varPulse ::varColor env =>
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
      let neuroColor = colorOf (varColor *. 5.);
      env |>
      Util.withContext (
        fun () => {
          Draw.fill neuroColor env;
          let scale = max 0. varPulse;
          let size = squareSize *. 0.1 +. squareSize *. scale;
          Util.drawRectfCenter width::size height::size env
        }
      )
    }
  );

let drawFitzhugh ::squareSize=200. (state: Fitzhugh.state) env =>
  Fitzhugh.(draw2d ::squareSize varPulse::state.v varColor::state.w env);

let drawMorrisLecar ::squareSize=200. (state: MorrisLecar.state) env =>
  MorrisLecar.(draw2d ::squareSize varPulse::state.v varColor::state.n env);
