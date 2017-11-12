open Reprocessing;

module Util = Hodgkin_Util;

module Color = Hodgkin_Color;

module Shape = Hodgkin_Shape;

module Fitzhugh = Hodgkin_Neuro_Fitzhugh;

let colorOf = (value) =>
  Utils.lerpColor(~low=Color.red, ~high=Color.blue, ~value);

let draw2d =
    (
      ~size=200.,
      ~varPulse,
      ~minPulse=(-0.2),
      ~maxPulse=1.,
      ~restPulse=0.,
      ~varColor,
      ~minColor=0.,
      ~maxColor=1.,
      ~restColor=0.,
      env
    ) =>
  Util.withContext(
    () => {
      Util.withContext(
        () => {
          Draw.fill(Color.grayScale(0.7), env);
          Draw.stroke(Color.grayScale(0.5), env);
          Draw.strokeWeight(1, env);
          Draw.ellipsef(
            ~center=(0., 0.),
            ~radx=size /. 2.,
            ~rady=size /. 2.,
            env
          )
        },
        env
      );
      let neuroColor =
        colorOf(Utils.norm(~value=varColor, ~low=minColor, ~high=maxColor));
      Util.withContext(
        () => {
          Draw.fill(neuroColor, env);
          let scale = max(0., varPulse);
          let size =
            size
            *. 0.1
            +. size
            *. Utils.lerpf(~low=0., ~high=0.9, ~value=scale);
          Draw.ellipsef(
            ~center=(0., 0.),
            ~radx=size /. 2.,
            ~rady=size /. 2.,
            env
          )
        },
        env
      );
      Util.withContext(
        () => {
          let tmp_x = -. size /. 2.;
          Draw.translate(~x=tmp_x, ~y=25., env);
          Shape.(
            meter(
              ~direction=Up,
              ~meterColor=Color.redA(0.9),
              ~outlineColor=Color.grayScale(~a=0.7, 0.),
              ~backgroundColor=Some(Color.blackA(0.5)),
              ~width=10.,
              ~height=50.,
              ~min=minPulse,
              ~max=maxPulse,
              varPulse,
              env
            )
          );
          Draw.translate(~x=15., ~y=0., env);
          Shape.(
            meter(
              ~direction=Up,
              ~meterColor=Color.blueA(0.9),
              ~outlineColor=Color.grayScale(~a=0.7, 0.),
              ~backgroundColor=Some(Color.blackA(0.5)),
              ~width=10.,
              ~height=50.,
              ~min=minColor,
              ~max=maxColor,
              varColor,
              env
            )
          )
        },
        env
      )
    },
    env
  );

let drawFitzhugh = (~size=200., state: Fitzhugh.state, env) =>
  Fitzhugh.(draw2d(~size, ~varPulse=state.v, ~varColor=state.w *. 5., env));


/***
 https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods#Explicit_Runge.E2.80.93Kutta_methods
 */
type butcherTableau = {
  rkMat: array(array(float)),
  weights: array(float)
  /* nodes: array float */
};

let euler_tableau = {rkMat: [|[||]|], weights: [|1.|]};

let rk4_tableau = {
  rkMat: [|[||], [|0.5|], [|0., 0.5|], [|0., 0., 1.|]|],
  weights: {
    let _1_6 = 1. /. 6.;
    let _1_3 = 1. /. 3.;
    [|_1_6, _1_3, _1_3, _1_6|]
  }
};

let dopri_tableau = {
  rkMat: [|
    [||],
    [|1. /. 5.|],
    [|3. /. 40., 9. /. 40.|],
    [|44. /. 45., (-56.) /. 15., 32. /. 9.|],
    [|19372. /. 6561., (-25360.) /. 2187., 64448. /. 6561., (-212.) /. 729.|],
    [|
      9017. /. 3168.,
      (-355.) /. 33.,
      46732. /. 5247.,
      49. /. 176.,
      (-5103.) /. 18656.
    |],
    [|
      35. /. 384.,
      0.,
      500. /. 1113.,
      125. /. 192.,
      (-2187.) /. 6784.,
      11. /. 84.
    |]
  |],
  weights: [|
    35. /. 384.,
    0.,
    500. /. 1113.,
    125. /. 192.,
    (-2187.) /. 6784.,
    11. /. 84.,
    0.
  |]
};

module type ODE = {
  type state;
  type deriv;
  let zero_deriv: deriv;
  let scale_deriv: (deriv, float) => deriv;
  let add_derivs: (deriv, deriv) => deriv;
  let step: (state, deriv, float) => state;
};

module Sim = (F: ODE) => {
  /* Explicit Runge-Kutta method
     https://en.wikipedia.org/wiki/Runge%E2%80%93Kutta_methods#Explicit_Runge.E2.80.93Kutta_methods
     */
  let stepExplicitRungeKutta =
      (
        ~tableau as {rkMat, weights}: butcherTableau,
        ~t,
        f: F.state => F.deriv,
        st: F.state
      )
      : F.state => {
    let s = Array.length(rkMat);
    assert (s == Array.length(weights));
    let k: array(F.deriv) = Array.make(s, F.zero_deriv);
    for (i in 0 to s - 1) {
      assert (i == Array.length(rkMat[i]));
      let accum_slope = ref(F.zero_deriv);
      for (j in 0 to i - 1) {
        accum_slope := F.scale_deriv(accum_slope^, rkMat[i][j])
      };
      k[i] = f(F.step(st, accum_slope^, t))
    };
    let acc = ref(F.zero_deriv);
    weights
    |> Array.iteri(
         (i, w_i) => acc := F.add_derivs(acc^, F.scale_deriv(k[i], w_i))
       );
    F.step(st, acc^, t)
  };
  let stepRKEuler = stepExplicitRungeKutta(~tableau=euler_tableau);
  let stepRK4 = stepExplicitRungeKutta(~tableau=rk4_tableau);
  let stepDoPri = stepExplicitRungeKutta(~tableau=dopri_tableau);
  let stepEuler = (~t, f: F.state => F.deriv, st: F.state) : F.state =>
    F.step(st, f(st), t);
};

module MorrisLecar = {
  include Hodgkin_Neuro_MorrisLecar;
  include Sim(Hodgkin_Neuro_MorrisLecar);
};

let drawMorrisLecar = (~size=200., state: MorrisLecar.state, env) =>
  MorrisLecar.(
    draw2d(
      ~size,
      ~varPulse=Utils.norm(~low=minV, ~high=maxV, ~value=state.v),
      ~varColor=Utils.norm(~low=minN, ~high=maxN, ~value=state.n),
      env
    )
  );
