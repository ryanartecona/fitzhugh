open Reprocessing;

let withContext f env => {
  Draw.pushStyle env;
  Draw.pushMatrix env;
  f ();
  Draw.popStyle env;
  Draw.popMatrix env
};

let drawRectCenter ::width ::height env =>
  Draw.rect pos::(- width / 2, - height / 2) ::width ::height env;

let drawRectfCenter ::width ::height env =>
  Draw.rectf pos::(-. width /. 2., -. height /. 2.) ::width ::height env;

/* https://www.graphpad.com/guides/prism/7/curve-fitting/reg_classic_boltzmann.htm?toc=0&printWindow
 * https://en.wikipedia.org/wiki/Logistic_function
 * https://en.wikipedia.org/wiki/Generalised_logistic_function
 */
let logisticCurve ::min=0. ::max=1. ::midValue=0. ::slopeInv=1. value =>
  min +. (max -. min) /. (1. +. exp ((midValue -. value) /. slopeInv));
