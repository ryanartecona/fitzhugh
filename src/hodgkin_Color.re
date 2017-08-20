open Reprocessing.Constants;

open Reprocessing.Utils;

let grayScale x => lerpColor low::white high::black value::x;

let gray = grayScale 0.5;

let darkGray = grayScale 0.7;

let white = grayScale 0.;

let black = grayScale 1.;

let orange = color r::0xCC g::0x55 b::0x11;

let red = color r::0xCC g::0x22 b::0x55;

let blue = color r::0x55 g::0x99 b::0xCC;
