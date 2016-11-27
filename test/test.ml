(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

open Depyt

type r = { foo: int; bar: string list; z: z option }
and z = { x: int; r: r list }

let r, z =
  mu2 (fun r z ->
      record "r" (fun foo bar z -> { foo; bar; z })
      |+ field "foo" int (fun t -> t.foo)
      |+ field "bar" (list string) (fun t -> t.bar)
      |+ field "z" (option z) (fun t -> t.z)
      |> sealr,
      record "z" (fun x r -> { x; r })
      |+ field "x" int (fun t -> t.x)
      |+ field "r" (list r) (fun t -> t.r)
      |> sealr
    )

let r1 = { foo = 3; bar = ["aaa";"b"]; z = None }
let r2 = { foo = 3; bar = ["aaa";"c"]; z = Some { x = 2; r = [r1; r1] } }

type v =
  | Foo
  | Bar of int
  | Yo of x * v option

and x = {
  r: r;
  i: (int * v) list;
}

let mkv v x =
  variant "v" (fun foo bar toto -> function
    | Foo         -> foo
    | Bar x       -> bar x
    | Yo (x, y) -> toto (x, y))
  |~ case0 "Foo" Foo
  |~ case1 "Bar" int (fun x -> Bar x)
  |~ case1 "Yo" (pair x (option v)) (fun (x, y) -> Yo (x, y))
  |> sealv

let mkx v =
  record "x" (fun r i -> { r; i })
  |+ field "r" r (fun x -> x.r)
  |+ field "i" (list (pair int v)) (fun x -> x.i)
  |> sealr

let v, x = mu2 (fun v x -> mkv v x, mkx v)

let v1 = Foo
let v2 = Bar 0
let v3 =
  Yo ({ r = r2; i = [ (1, v1); (2, v2); (3, v2); (4, Bar 3); (5, Bar 6)] },
      Some v2)

type my_e = Fooe | Bars | Toto | Tata
let e = enum "e" ["Fooe", Fooe; "Bars", Bars; "Toto", Toto; "Tata", Tata]

let e1 = Fooe
let e2 = Bars
let e3 = Toto
let e4 = Tata

(* FIXME: should go upstream *)
let neg t =
  Alcotest.testable (Alcotest.pp t) (fun x y -> not (Alcotest.equal t x y))

let test t = Alcotest.testable (pp t) (equal t)

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
  Alcotest.(check @@ neg @@ test v) __LOC__ v3 v1;
  Alcotest.(check @@ test e) __LOC__ e1 e1;
  Alcotest.(check @@ neg @@ test e) __LOC__ e1 e2;
  Alcotest.(check @@ neg @@ test e) __LOC__ e1 e3

let test_compare () =
  Alcotest.(check int) __LOC__ (compare r r1 r2) ~-1;
  Alcotest.(check int) __LOC__ (compare v v1 v2) ~-1;
  Alcotest.(check int) __LOC__ (compare v v2 v3)  ~-1;
  Alcotest.(check int) __LOC__ (compare v v3 v1)  1;
  Alcotest.(check int) __LOC__ (compare e e1 e2) ~-1;
  Alcotest.(check int) __LOC__ (compare e e2 e3) ~-1;
  Alcotest.(check int) __LOC__ (compare e e3 e4) ~-1;
  Alcotest.(check int) __LOC__ (compare e e4 e1) 1

let test_bin_write () =
  let check t x =
    let len = Bin.size_of t x in
    let buf = Cstruct.create len in
    let len'= Bin.write t buf ~pos:0 x in
    let msg = Fmt.strf "%a\n%s" (pp t) x in
    Alcotest.(check int) (msg __LOC__) len len'
  in
  check r r1;
  check r r2;
  check v v1;
  check v v2;
  check v v3;
  check e e1;
  check e e2;
  check e e3

let test_bin_read () =
  let check t x =
    let len = Bin.size_of t x in
    let buf = Cstruct.create len in
    let len' = Bin.write t buf ~pos:0 x in
    Alcotest.(check int) __LOC__ len len';
    let len', y = Bin.read t buf ~pos:0 in
    Alcotest.(check int) __LOC__ len len';
    Alcotest.(check @@ test t) __LOC__ x y
  in
  check r r1;
  check r r2;
  check v v1;
  check v v2;
  check v v3;
  check e e1;
  check e e2;
  check e e3

let () =
  Alcotest.run "depyt" [
    "basic", [
      "equal"  , `Quick, test_equal;
      "compare", `Quick, test_compare;
      "write"  , `Quick, test_bin_write;
      "read"   , `Quick, test_bin_read;
    ]
  ]

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
