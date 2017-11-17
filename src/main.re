open Reprocessing;

open Hodgkin;

type modelState = {exc1: Neuro.MorrisLecar.state};

type state = {
  modelState,
  modelMonitorLine: Shape.monitorLine,
  inputMonitorLine: Shape.monitorLine
};

let (minInput, maxInput) = (12., 50.);

let setup = (env) => {
  Env.size(~width=600, ~height=400, env);
  {
    modelState: {exc1: Neuro.MorrisLecar.initState},
    modelMonitorLine:
      Neuro.MorrisLecar.{
        min: minV,
        max: maxV,
        color: Utils.lerpColor(~low=Color.red, ~high=Color.white, ~value=0.2),
        strokeWeight: 2,
        points: [||],
        maxPointCount: 400
      },
    inputMonitorLine: {
      min: minInput,
      max: maxInput,
      color: Color.grayScale(~a=0.8, 0.5),
      strokeWeight: 4,
      points: [||],
      maxPointCount: 400
    }
  }
};

let draw = (state, env) => {
  let (frameWidth, frameHeight) = (Env.width(env), Env.height(env));
  let {modelMonitorLine, inputMonitorLine} = state;
  let {exc1} = state.modelState;
  let neuronSize = 150.;
  Draw.background(Constants.black, env);
  let monitorNormMouseY =
    Util.withContext(
      () => {
        Draw.translate(~x=10., ~y=float_of_int(frameHeight) -. 90., env);
        Shape.(
          monitor(
            ~width=float_of_int(frameWidth - 20),
            ~height=80.,
            [inputMonitorLine, modelMonitorLine],
            env
          )
        )
      },
      env
    );
  Util.withContext(
    () => {
      Draw.translate(
        ~x=float_of_int(frameWidth / 2),
        ~y=float_of_int(frameHeight / 2),
        env
      );
      Neuro.drawMorrisLecar(~size=neuronSize, exc1, env)
    },
    env
  );
  let staticInput = minInput;
  let rampedInput =
    switch monitorNormMouseY {
    | None => staticInput
    | Some(value) => Utils.lerpf(~value, ~low=minInput, ~high=maxInput)
    };
  let nextExc1 =
    Neuro.MorrisLecar.(
      Util.iterN(40, exc1, stepRK4(slope(~input=rampedInput), ~t=0.004))
    );
  {
    modelState: {exc1: nextExc1},
    modelMonitorLine: Shape.appendDatum(modelMonitorLine, exc1.v),
    inputMonitorLine: Shape.appendDatum(inputMonitorLine, rampedInput)
  }
};

run(~setup, ~draw, ());
