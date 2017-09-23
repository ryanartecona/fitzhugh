open Reprocessing;

module Util = Hodgkin_Util;

module Color = Hodgkin_Color;

module Shape = Hodgkin_Shape;

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
  Util.withContext
    (
      fun () => {
        Util.withContext
          (
            fun () => {
              Draw.fill (Color.grayScale 0.7) env;
              Draw.stroke (Color.grayScale 0.5) env;
              Draw.strokeWeight 1 env;
              Draw.ellipsef
                center::(0., 0.) radx::(size /. 2.) rady::(size /. 2.) env
            }
          )
          env;
        let neuroColor =
          colorOf (Utils.norm value::varColor low::minColor high::maxColor);
        Util.withContext
          (
            fun () => {
              Draw.fill neuroColor env;
              let scale = max 0. varPulse;
              let size =
                size
                *. 0.1
                +. size
                *. Utils.lerpf low::0. high::0.9 value::scale;
              Draw.ellipsef
                center::(0., 0.) radx::(size /. 2.) rady::(size /. 2.) env
            }
          )
          env;
        Util.withContext
          (
            fun () => {
              Draw.translate x::(-. size /. 2.) y::25. env;
              Shape.(
                meter
                  direction::Up
                  meterColor::(Color.redA 0.9)
                  outlineColor::(Color.grayScale a::0.7 0.)
                  backgroundColor::(Some (Color.blackA 0.5))
                  width::10.
                  height::50.
                  min::minPulse
                  max::maxPulse
                  varPulse
                  env
              );
              Draw.translate x::15. y::0. env;
              Shape.(
                meter
                  direction::Up
                  meterColor::(Color.blueA 0.9)
                  outlineColor::(Color.grayScale a::0.7 0.)
                  backgroundColor::(Some (Color.blackA 0.5))
                  width::10.
                  height::50.
                  min::minColor
                  max::maxColor
                  varColor
                  env
              )
            }
          )
          env
      }
    )
    env;

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
