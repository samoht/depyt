#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "depyt" @@ fun c ->
  Ok [ Pkg.mllib "src/depyt.mllib";
       Pkg.test "test/test"; ]
