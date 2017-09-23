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
let meter
    ::direction=Up
    ::meterColor=red
    ::outlineColor=black
    ::backgroundColor=None
    ::width=10.
    ::height=100.
    ::min=0.
    ::max=1.
    value
    env => {
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
    | Down => (1., Pervasives.min 1. (Utils.norm ::value low::min high::max))
    | Left
    | Right => (Pervasives.min 1. (Utils.norm ::value low::min high::max), 1.)
    };
  withContext
    (
      fun () => {
        Draw.scale x::scaleX y::scaleY env;
        let _ =
          switch backgroundColor {
          | None => Draw.noFill env
          | Some bg => Draw.fill bg env
          };
        Draw.rectf pos::(0., 0.) ::width ::height env;
        Draw.noStroke env;
        Draw.fill meterColor env;
        Draw.rectf
          pos::(0., 0.)
          width::(width *. scaleWidth)
          height::(height *. scaleHeight)
          env;
        Draw.stroke outlineColor env;
        Draw.strokeWeight 1 env;
        Draw.noFill env;
        Draw.rectf pos::(0., 0.) ::width ::height env
      }
    )
    env;
  ()
};
