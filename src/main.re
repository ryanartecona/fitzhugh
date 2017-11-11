open Reprocessing;

open Hodgkin;

type modelState = {exc1: Neuro.MorrisLecar.state};

type state = {
  modelState,
  input: float
};

let setup = (env) => {
  Env.size(~width=600, ~height=400, env);
  {modelState: {exc1: Neuro.MorrisLecar.initState}, input: 0.}
};

let draw = (state, env) => {
  let (frameWidth, frameHeight) = (Env.width(env), Env.height(env));
  let mouseIsDown = Env.mousePressed(env);
  let (minInput, maxInput) = (12., 50.);
  let {input} = state;
  let {exc1} = state.modelState;
  let neuronSize = 150.;
  Draw.background(Constants.black, env);
  env
  |> Util.withContext(
       () => {
         /* draw a meter that shows ramped input current */
         Draw.translate(~x=10., ~y=10., env);
         Shape.(
           meter(
             ~direction=Right,
             ~outlineColor=Color.grayScale(0.2),
             ~width=100.,
             ~height=20.,
             ~min=minInput,
             ~max=maxInput,
             input,
             env
           )
         )
       }
     );
  env
  |> Util.withContext(
       () => {
         Draw.translate(~x=float_of_int(frameWidth / 2), ~y=float_of_int(frameHeight / 2), env);
         Neuro.drawMorrisLecar(~size=neuronSize, exc1, env)
       }
     );
  let staticInput = minInput;
  let rampedInput =
    if (mouseIsDown) {
      min(maxInput, input +. 0.03)
    } else {
      staticInput
    };
  let nextExc1 =
    Neuro.MorrisLecar.(
      Util.iterN(80, array_of_state(exc1), Neuro.stepRK4(slope(~input=rampedInput), ~t=0.001))
      |> state_of_array
    );
  {modelState: {exc1: nextExc1}, input: rampedInput}
};

run(~setup, ~draw, ());
