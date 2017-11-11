open Reprocessing;

module Util = Hodgkin_Util;

module Color = Hodgkin_Color;

module Shape = Hodgkin_Shape;

module Fitzhugh = Hodgkin_Neuro_Fitzhugh;

module MorrisLecar = Hodgkin_Neuro_MorrisLecar;

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

let drawMorrisLecar = (~size=200., state: MorrisLecar.state, env) =>
  MorrisLecar.(
    draw2d(
      ~size,
      ~varPulse=Utils.norm(~low=minV, ~high=maxV, ~value=state.v),
      ~varColor=Utils.norm(~low=minN, ~high=maxN, ~value=state.n),
      env
    )
  );

let stepEuler =
    (
      ~t,
      f: array(float) /* state array */ => array(float) /* deriv array */,
      st: array(float)
    )
    : array(float) => {
  let d = f(st);
  assert (Array.length(st) == Array.length(d));
  st |> Array.mapi((i, st_i) => st_i +. d[i] *. t)
};


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

let mult_array_constant = (arr, c) => Array.map((a) => a *. c, arr);

let add_arrays = (arr1, arr2) =>
  Array.mapi((i, arr1_i) => arr1_i +. arr2[i], arr1);

let mult_arrays = (arr1, arr2) =>
  Array.mapi((i, arr1_i) => arr1_i *. arr2[i], arr1);

let zero_array = (n) => Array.make(n, 0.);

let stepExplicitRungeKutta =
    (
      ~tableau as {rkMat, weights}: butcherTableau,
      ~t,
      f: array(float) /* state array */ => array(float) /* deriv array */,
      st: array(float)
    )
    : array(float) => {
  let st_len = Array.length(st);
  let d = f(st);
  assert (Array.length(d) == st_len);
  let s = Array.length(rkMat);
  assert (s == Array.length(weights));
  /* assert (s == Array.length tab.nodes); */
  let k: array(array(float)) = Array.init(s, (_) => [||]);
  for (i in 0 to s - 1) {
    assert (i == Array.length(rkMat[i]));
    let accum_slope = ref(zero_array(st_len));
    for (j in 0 to i - 1) {
      accum_slope := mult_array_constant(accum_slope^, rkMat[i][j])
    };
    k[i] = f(add_arrays(st, mult_array_constant(accum_slope^, t)))
  };
  let acc = ref(zero_array(st_len));
  weights
  |> Array.iteri(
       (i, w_i) => acc := add_arrays(acc^, mult_array_constant(k[i], w_i))
     );
  add_arrays(st, mult_array_constant(acc^, t))
};

let stepRKEuler = stepExplicitRungeKutta(~tableau=euler_tableau);

let stepRK4 = stepExplicitRungeKutta(~tableau=rk4_tableau);
