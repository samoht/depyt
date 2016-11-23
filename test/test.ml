(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

open Depyt
open Result

type my_r = { foo: int; bar: string list }

let r =
  let foo = field "foo" int (fun t -> t.foo) in
  let bar = field "bar" (list string) (fun t -> t.bar) in
  record2 "r" foo bar @@ fun foo bar -> { foo; bar }

type my_v =
  | Foo
  | Bar of int

let v =
  let foo, mkfoo = case0 "Foo" Foo in
  let bar, mkbar = case1 "Bar" int (fun x -> Bar x) in
  variant "v" [foo; bar] (function Foo -> mkfoo | Bar x -> mkbar x)

let r1 = { foo = 3; bar = ["aaa";"b"] }
let r2 = { foo = 3; bar = ["aaa";"c"] }
let v1 = Foo
let v2 = Bar 0
let v3 = Bar 1

(* FIXME: should go upstream *)
let neg t =
  Alcotest.testable (Alcotest.pp t) (fun x y -> not (Alcotest.equal t x y))

let test_equal () =
  Alcotest.(check @@ test r) __LOC__ r1 r1;
  Alcotest.(check @@ test r) __LOC__ r2 r2;
  Alcotest.(check @@ test v) __LOC__ v1 v1;
  Alcotest.(check @@ test v) __LOC__ v2 v2;
  Alcotest.(check @@ test v) __LOC__ v3 v3;
  Alcotest.(check @@ neg @@ test r) __LOC__ r1 r2;
  Alcotest.(check @@ neg @@ test r) __LOC__ r2 r1;
  Alcotest.(check @@ neg @@ test v) __LOC__ v1 v2;
  Alcotest.(check @@ neg @@ test v) __LOC__ v2 v3;
  Alcotest.(check @@ neg @@ test v) __LOC__ v3 v1

let test_compare () =
  Alcotest.(check int) __LOC__ (compare r r1 r2) ~-1;
  Alcotest.(check int) __LOC__ (compare v v1 v2) ~-1;
  Alcotest.(check int) __LOC__ (compare v v2 v3)  ~-1;
  Alcotest.(check int) __LOC__ (compare v v3 v1)  1

let test_write () =
  let check t x =
    let len = size_of t x in
    let buf0 = Cstruct.create len in
    let buf = write t x buf0 in
    Alcotest.(check int) (Fmt.to_to_string (pp t) x) (Cstruct.len buf) 0
  in
  check r r1;
  check r r2;
  check v v1;
  check v v2;
  check v v3

let test_read () =
  let check t x =
    let len = size_of t x in
    let buf0 = Cstruct.create len in
    let buf = write t x buf0 in
    Alcotest.(check int) __LOC__ (Cstruct.len buf) 0;
    match read t (Mstruct.of_cstruct buf0) with
    | Ok y    -> Alcotest.(check @@ test t) __LOC__ x y
    | Error _ -> Alcotest.fail __LOC__
  in
  check r r1;
  check r r2;
  check v v1;
  check v v2;
  check v v3

let () =
  Printexc.record_backtrace true;
  Alcotest.run "depyt" [
    "basic", [
      "equal"  , `Quick, test_equal;
      "compare", `Quick, test_compare;
      "write"  , `Quick, test_write;
      "read"   , `Quick, test_read;
    ]
  ]
