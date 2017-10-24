open Alcotest;

let qed = ("QED.", `Quick, fun () => check int "0=0" 0 0);

module HodgkinTests = {
  module Util = {
    let test_iterN () => {
      let count = Hodgkin.Util.iterN 20 0 (fun x => x + 10);
      check int "counted 20 times" count 200
    };
  };
};

let util_tests = (
  "Hodgkin.Util",
  HodgkinTests.Util.[("iterN", `Quick, test_iterN)]
);

run "hodgkin" [("hello", [qed]), util_tests];
