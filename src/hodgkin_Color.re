open Reprocessing.Constants;

open Reprocessing.Utils;

let gray = color r::0x88 g::0x88 b::0x88;

let darkGray = color r::0x55 g::0x55 b::0x55;

let grayScale x => lerpColor low::white high::black value::x;

let orange = color r::0xCC g::0x55 b::0x11;

let red = color r::0xCC g::0x22 b::0x55;

let blue = color r::0x55 g::0x99 b::0xCC;
