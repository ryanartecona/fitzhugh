let qed = ("QED.", `Quick, fun () => Alcotest.(check int) "0=0" 0 0);

module HodgkinTests = {
  module Util = {
    let test_iterN () => {
      let count = Hodgkin.Util.iterN 20 0 (fun x => x + 10);
      Alcotest.(check int) "counted 20 times" count 200
    };
  };
};

let util_tests = (
  "Hodgkin.Util",
  HodgkinTests.Util.[("iterN", `Quick, test_iterN)]
);

Alcotest.run "hodgkin" [("hello", [qed]), util_tests];
