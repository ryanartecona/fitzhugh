open Reprocessing;

open Hodgkin_Util;

open Hodgkin_Color;

type meterDirection =
  | Up
  | Down
  | Left
  | Right;

/*
 Draw a meter to display a bounded scalar value growing in the chosen direction.
 The meter is anchored at the top left corner in the Down and Right directions,
 and flipped horizontally for Left and vertically for Up.
 */
let meter =
    (
      ~direction=Up,
      ~meterColor=red,
      ~outlineColor=black,
      ~backgroundColor=None,
      ~width=10.,
      ~height=100.,
      ~min=0.,
      ~max=1.,
      value,
      env
    ) => {
  let (scaleX, scaleY) =
    switch direction {
    | Up => (1., (-1.))
    | Down => (1., 1.)
    | Left => ((-1.), 1.)
    | Right => (1., 1.)
    };
  let (scaleWidth, scaleHeight) =
    switch direction {
    | Up
    | Down => (1., Pervasives.min(1., Utils.norm(~value, ~low=min, ~high=max)))
    | Left
    | Right => (
        Pervasives.min(1., Utils.norm(~value, ~low=min, ~high=max)),
        1.
      )
    };
  withContext(
    () => {
      Draw.scale(~x=scaleX, ~y=scaleY, env);
      let _ =
        switch backgroundColor {
        | None => Draw.noFill(env)
        | Some(bg) => Draw.fill(bg, env)
        };
      Draw.rectf(~pos=(0., 0.), ~width, ~height, env);
      Draw.noStroke(env);
      Draw.fill(meterColor, env);
      Draw.rectf(
        ~pos=(0., 0.),
        ~width=width *. scaleWidth,
        ~height=height *. scaleHeight,
        env
      );
      Draw.stroke(outlineColor, env);
      Draw.strokeWeight(1, env);
      Draw.noFill(env);
      Draw.rectf(~pos=(0., 0.), ~width, ~height, env)
    },
    env
  );
  ()
};

type monitorLine = {
  min: float,
  max: float,
  color: Reprocessing.colorT,
  strokeWeight: int,
  points: array(float),
  maxPointCount: int
};

let monitor =
    (
      ~width=100.,
      ~height=30.,
      ~outlineColor=grayScale(~a=0.3, 0.),
      ~backgroundColor=grayScale(~a=0.3, 0.5),
      lines,
      env
    )
    : option(float) =>
  withContext(
    () => {
      let (canvasMouseX, canvasMouseY) = Env.(mouse(env));
      let (mouseX, mouseY) =
        Env.localizePointf(
          (float_of_int(canvasMouseX), float_of_int(canvasMouseY)),
          env
        );
      let mouseIsDown = Env.mousePressed(env);
      Draw.noStroke(env);
      let ret =
        mouseIsDown
        && mouseX > 0.
        && mouseX < width
        && mouseY > 0.
        && mouseY < height ?
          {
            Draw.fill(black, env);
            Some(Utils.norm(~value=mouseY, ~high=0., ~low=height))
          } :
          {
            Draw.fill(backgroundColor, env);
            None
          };
      Draw.rectf(~pos=(0., 0.), ~width, ~height, env);
      lines
      |> List.iter(
           (line) => {
             let pointCount = Array.length(line.points);
             let missingPointCount = line.maxPointCount - pointCount;
             let stepSizePx = width /. float_of_int(line.maxPointCount - 1);
             Draw.stroke(line.color, env);
             Draw.strokeWeight(line.strokeWeight, env);
             Draw.strokeCap(Square, env);
             for (i in 0 to pointCount - 1) {
               let p = line.points[i];
               switch line.points[i + 1] {
               | nextPoint =>
                 let x = stepSizePx *. float_of_int(i + missingPointCount);
                 let y =
                   height
                   -. height
                   *. Utils.norm(~value=p, ~low=line.min, ~high=line.max);
                 let nextX = x +. stepSizePx;
                 let nextY =
                   height
                   -. height
                   *. Utils.norm(
                        ~value=nextPoint,
                        ~low=line.min,
                        ~high=line.max
                      );
                 Draw.linef(~p2=(x, y), ~p1=(nextX, nextY), env);
                 ()
               | exception (Invalid_argument(_)) => ()
               }
             }
           }
         );
      Draw.stroke(outlineColor, env);
      Draw.strokeWeight(1, env);
      Draw.noFill(env);
      Draw.rectf(~pos=(0., 0.), ~width, ~height, env);
      ret
    },
    env
  );

let appendDatum = (line, datum) => {
  let appended = Array.append(line.points, [|datum|]);
  {
    ...line,
    points:
      Array.length(appended) > line.maxPointCount ?
        Array.sub(appended, 1, line.maxPointCount) : appended
  }
};
