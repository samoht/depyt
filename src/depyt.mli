(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Yet-an-other type combinator library

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Depyt} *)

type 'a t
(** The type for runtime representation of values of type ['a]. *)

(** {1 Primitives} *)

val unit: unit t
(** [unit] is a representation of the unit type. *)

val int: int t
(** [int] is a representation of the integer type. *)

val string: string t
(** [string] is a representation of the string type. *)

val list: 'a t -> 'a list t
(** [list t] is a representation of list of values of type [t]. *)

val option: 'a t -> 'a option t
(** [option t] is a representation of value of type [t option]. *)

val pair: 'a t -> 'b t -> ('a * 'b) t
(** [pair x y] is a representation of values of type [x * y]. *)

(** {1 Records} *)

type ('a, 'b) field
(** The type for fields holding values of type ['b] and belonging to a
    record of type ['a]. *)

val field: string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
(** [field1 n t g] is the representation of the field [n] of type [t]
    with getter [g]. *)

val record1: string -> ('a, 'b) field -> ('b -> 'a) -> 'a t
(** [record1 n f mk] is the representation of the record called [n] of
    type ['a], having only one field of type ['b] and with constructor
    [c].

    For instance:

    {[
      type t = { foo: string }

      let t =
        record1 "t" (field "foo" string (fun t -> t.foo))
        @@ fun foo -> { foo }
    ]}
*)

val record2:
  string ->
  ('a, 'b) field ->
  ('a, 'c) field ->
  ('b -> 'c -> 'a) -> 'a t
(** Same as {!record1} but for records with 2 fields. e.g.

    {[
      type t = { foo: string; bar = int list }

      let t =
        record2 "t"
          (field "foo" string (fun t -> t.foo))
          (field "bar" (list int) (fun t -> t.bar))
        @@ fun foo bar -> { foo; bar }
    ]}
*)

val record3:
  string ->
  ('a, 'b) field ->
  ('a, 'c) field ->
  ('a, 'd) field ->
  ('b -> 'c -> 'd -> 'a) -> 'a t
(** Same as {!record1} but for records with 3 fields. *)

val record4:
  string ->
  ('a, 'b) field ->
  ('a, 'c) field ->
  ('a, 'd) field ->
  ('a, 'e) field ->
  ('b -> 'c -> 'd -> 'e -> 'a) -> 'a t
(** Same as {!record1} but for records with 4 fields. *)

val record5:
  string ->
  ('a, 'b) field ->
  ('a, 'c) field ->
  ('a, 'd) field ->
  ('a, 'e) field ->
  ('a, 'f) field ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'a t
(** Same as {!record1} but for records with 5 fields. *)

val record6:
  string ->
  ('a, 'b) field ->
  ('a, 'c) field ->
  ('a, 'd) field ->
  ('a, 'e) field ->
  ('a, 'f) field ->
  ('a, 'g) field ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) -> 'a t
(** Same as {!record1} but for records with 6 fields. *)

(** {1 Variants} *)

type 'a case
(** The type for representing variant cases. *)

type 'a case0
(** The type for representing a variant of type ['a] with 0 arguments,
    e.g. [Foo]. *)

type ('a, 'b) case1
(** The type for representing a variant of type ['a] with 1 argument
    of type ['b], e. g. [Foo of 'b]. *)

val case0: string -> 'a -> 'a case * 'a case0
(** [case0 n v] is a representation of a variant case [n] with no
    argument. e.g.

    {[
      type t = Foo
      let foo = case0 "Foo" Foo
    ]}
    *)

val case1: string -> 'b t -> ('b -> 'a) -> 'a case * ('b -> 'a case0)
(** [case1 n t c] is a representation of a variant case [n] with 1
    argument of type [t] and constructor [c]. e.g.

    {[
      type t = Foo of string
      let foo = case1 "Foo" string (fun s -> Foo s)
    ]}
*)

val variant: string -> 'a case list -> ('a -> 'a case0) -> 'a t
(** [variant n c p] is a representation of a variant type containing
    the cases [c] and using [p] to deconstruct values, e.g.

    {[
      type t = Foo | Bar of string
      let t =
        let foo, mkfoo = case0 "Foo" Foo in
        let bar, mkbar = case1 "Bar" string (fun x -> Bar x) in
        variant "t" [foo; bar]
        @@ function Foo -> mkfoo | Bar x -> mkbar x
    ]}

  *)

val enum: string -> (string * 'a) list -> 'a t
(** [enum n l] is a representation of the variant type which has
    only constant variant case. e.g.

    {[
      type t = Foo | Bar | Toto
      let t = enum "t" ["Foo", Foo; "Bar", Bar; "Toto", Toto]
    ]}
*)

(** {1 Operations}

    Given a value ['a t], it is possible to define high-level
    operations on value of type ['a] such as pretty-printing, parsing
    and unparsing. We provide here few examples.
*)

val pp: 'a t -> 'a Fmt.t
(** [pp t] is the pretty-printer for values of type [t]. *)

val equal: 'a t -> 'a -> 'a -> bool
(** [equal t] is the equality function between values of type [t]. *)

val compare: 'a t -> 'a -> 'a -> int
(** [compare t] compares values of type [t]. *)

val size_of: 'a t -> 'a -> int
(** [size_of t] is the size needed to serialize values of type [t]. *)

val write: 'a t -> 'a -> Cstruct.t -> Cstruct.t
(** [write t] serializes values of type [t]. Use [size_of] to
    pre-determine the size of the buffer. *)

val read: 'a t -> Mstruct.t -> ('a, string) result
(** [read t] reads a serialization of a value of type [t]. *)

val test: 'a t -> 'a Alcotest.testable
(** [test t] is a test check for values of type [t]. *)

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
