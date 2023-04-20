open Base

let count_words words =
  let map = Map.empty (module String.Caseless) in
  List.fold words ~init:map ~f:(fun acc e ->
    Map.update acc e ~f:(function
      | None -> 1
      | Some n -> n + 1))
  |> Map.to_alist ~key_order:`Increasing
;;

let () =
  let test case_name input expected =
    Alcotest.test_case case_name `Quick (fun () ->
      Alcotest.(check (list (pair string int))) "equal lists" expected (count_words input))
  in
  Alcotest.run
    "Tests"
    [ ( "map key test"
      , [ test "simple" [ "Foo" ] [ "Foo", 1 ]
        ; test "coalescing different cases" [ "Foo"; "foo" ] [ "Foo", 2 ]
        ; test "keys in increasing order" [ "Foo"; "bar" ] [ "bar", 1; "Foo", 1 ]
        ; test "preserves keys" [ "FOO"; "bar"; "foo" ] [ "bar", 1; "FOO", 2 ]
        ; test
            "second occurence always changes key"
            [ "FOO"; "bar"; "foo"; "BAR" ]
            [ "BAR", 2; "foo", 2 ]
        ] )
    ]
;;
