let qed = ("QED.", `Quick, fun () => Alcotest.(check int) "0=0" 0 0);

Alcotest.run "TESTS" [("test_set", [qed, qed])];
