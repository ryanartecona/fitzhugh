open Reprocessing;

module Util = Hodgkin_Util;

module Color = Hodgkin_Color;

module Fitzhugh = Hodgkin_Neuro_Fitzhugh;

module MorrisLecar = Hodgkin_Neuro_MorrisLecar;

let colorOf value => Utils.lerpColor low::Color.red high::Color.blue ::value;

let draw2d
    ::size=200.
    ::varPulse
    ::minPulse=(-0.2)
    ::maxPulse=1.
    ::restPulse=0.
    ::varColor
    ::minColor=0.
    ::maxColor=1.
    ::restColor=0.
    env =>
  env |>
  Util.withContext (
    fun () => {
      env |>
      Util.withContext (
        fun () => {
          Draw.fill Color.darkGray env;
          let scale = 1. +. max 0. (-. varPulse);
          let size = size *. scale;
          Draw.ellipsef
            center::(0., 0.) radx::(size /. 2.) rady::(size /. 2.) env
        }
      );
      Draw.fill Color.gray env;
      Draw.ellipsef center::(0., 0.) radx::(size /. 2.) rady::(size /. 2.) env;
      let neuroColor = colorOf varColor;
      env |>
      Util.withContext (
        fun () => {
          Draw.fill neuroColor env;
          let scale = max 0. varPulse;
          let size =
            size *. 0.1 +. size *. Utils.lerpf low::0. high::0.9 value::scale;
          Draw.ellipsef
            center::(0., 0.) radx::(size /. 2.) rady::(size /. 2.) env
        }
      )
    }
  );

let drawFitzhugh ::size=200. (state: Fitzhugh.state) env =>
  Fitzhugh.(draw2d ::size varPulse::state.v varColor::(state.w *. 5.) env);

let drawMorrisLecar ::size=200. (state: MorrisLecar.state) env =>
  MorrisLecar.(
    draw2d
      ::size
      varPulse::(Utils.norm low::minV high::maxV value::state.v)
      varColor::(Utils.norm low::minN high::maxN value::state.n)
      env
  );
