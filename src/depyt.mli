(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

(** Yet-an-other type combinator library

    [Depyt] provides type combinators to define runtime representation
    for OCaml types and {{!generics}generic operations} to manipulate
    values with a runtime type representation.

    The type combinators supports all the usual {{!primitives}type
    primitives} but also compact definitions of {{!records}records}
    and {{!variants}variants}. It also allows to define the runtime
    representation of {{!recursive}recursive types}.

    [Depyt] is a modern reboot of
    {{:https://github.com/mirage/dyntype}Dyntype} but using
    {{:https://en.wikipedia.org/wiki/Generalized_algebraic_data_type}GADT}s-based
    combinators instead of syntax-extensions. When we originally wrote
    [Dyntype] (in 2012) GADTs were not available in {i OCaml} and
    {{:https://github.com/ocaml/camlp4}camlp4} was everywhere -- this
    is not the case anymore. Finally, [Depyt] avoids some of the
    performance caveats present in [Dyntype] by avoiding allocating
    and converting between intermediate formats.

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Depyt} *)

type 'a t
(** The type for runtime representation of values of type ['a]. *)

(** {1:primitives Primitives} *)

val unit: unit t
(** [unit] is a representation of the unit type. *)

val bool: bool t
(** [bool] is a representation of the boolean type. *)

val char: char t
(** [char] is a representation of the character type. *)

val int: int t
(** [int] is a representation of the integer type. *)

val int32: int32 t
(** [int32] is a representation of the 32-bit integers type. *)

val int64: int64 t
(** [int64] is a representation of the 64-bit integer type. *)

val float: float t
(** [float] is a representation of the float type. *)

val string: string t
(** [string] is a representation of the string type. *)

val list: 'a t -> 'a list t
(** [list t] is a representation of list of values of type [t]. *)

val option: 'a t -> 'a option t
(** [option t] is a representation of value of type [t option]. *)

val pair: 'a t -> 'b t -> ('a * 'b) t
(** [pair x y] is a representation of values of type [x * y]. *)

(** {1:records Records} *)

type ('a, 'b) field
(** The type for fields holding values of type ['b] and belonging to a
    record of type ['a]. *)

val field: string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
(** [field n t g] is the representation of the field [n] of type [t]
    with getter [g].

    For instance:

    {[
    type t = { foo: string option }

    let foo = field "foo" (option string) (fun t -> t.x)]}
*)

type ('a, 'b, 'c) open_record
(** The type for representing open records of type ['a] with
    constructors of type ['b]. ['c] represents the fields missings to
    the record, e.g. an open record initially holds ['c = 'b] and it
    can can be {{!sealr}sealed} when ['c = 'a]. *)

val sealr: ('a, 'b, 'a) open_record -> 'a t
(** [sealr r] seal the open record [r]. *)

val (|+):
  ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record
(** [r |+ f] adds the field [f] to the open record [r]. *)

val record: string -> 'b -> ('a, 'b, 'b) open_record
(** [record n f fs] is the representation of the record called [n] of
    type ['a] using [f] as constructor and with the fields [fs].

    Putting all together:

    {[
      type t = { foo: string; bar = (int * string) list; }

      let t =
        record "t" (fun foo -> { foo })
        |+ field "foo" string (fun t -> t.foo)
        |+ field "bar" (list (pair int string)) (fun t -> t.bar)
        |> sealr]}
*)

(** {1:variants Variants} *)

type ('a, 'b) case
(** The type for representing variant cases of type ['a] with
    patterns of type ['b]. *)

type 'a case_p
(** The type for representing patterns for a variant of type ['a]. *)

val case0: string -> 'a -> ('a, 'a case_p) case
(** [case0 n v] is a representation of a variant case [n] with no
    argument and a singleton pattern. e.g.

    {[
      type t = Foo

      let foo = case0 "Foo" Foo]}
*)

val case1: string -> 'b t -> ('b -> 'a) -> ('a, 'b -> 'a case_p) case
(** [case1 n t c] is a representation of a variant case [n] with 1
    argument of type [t] and a pattern [c] an function with one argument
    of type [t]. e.g.

    {[
      type t = Foo of string

      let foo = case1 "Foo" string (fun s -> Foo s)]}
*)

type ('a, 'b, 'c) open_variant
(** The type for representing open variants of type ['a] with pattern
    matching of type ['b]. ['c] represents the missing cases for the
    variant, e.g. initially variant hols [c' = 'b] and it can be
    {{!sealv}sealed} when ['c = 'a].  *)

val sealv: ('a, 'b, 'a -> 'a case_p) open_variant -> 'a t
(** [sealv v] seals the open variant [v]. *)

val (|~):
  ('a, 'b, 'c -> 'd) open_variant -> ('a, 'c) case -> ('a, 'b, 'd) open_variant
(** [v |~ c] is [v] augmented with the case [c]. *)

val variant: string -> 'b -> ('a, 'b, 'b) open_variant
(** [variant n c p] is a representation of a variant type containing
    the cases [c] and using [p] to deconstruct values.

    Putting all together:

    {[
      type t = Foo | Bar of string

      let t =
        variant "t" (fun foo bar -> function
          | Foo   -> foo
          | Bar s -> bar s)
        |~ case0 "Foo" Foo
        |~ case1 "Bar" string (fun x -> Bar x)
        |> sealr]}
*)

val enum: string -> (string * 'a) list -> 'a t
(** [enum n l] is a representation of the variant type which has
    only constant variant case. e.g.

    {[
      type t = Foo | Bar | Toto

      let t = enum "t" ["Foo", Foo; "Bar", Bar; "Toto", Toto]]}
*)

(** {1:recursive Recursive definitions}

    [Depyt] allows to create a limited form of recursive records and
    variants.

    {b TODO}: describe the limitations, e.g. only regular recursion and no
    use of the generics inside the [mu*] functions and the usual
    caveats with recursive values (such as infinite loops on most of
    the generics which don't check sharing).

*)

val mu: ('a t -> 'a t) -> 'a t
(** [mu f] is the representation [r] such that [r = mu r].

    For instance:

    {[
      type x = { x: x option }

      let x = mu (fun x ->
          record "x" (fun x -> { x })
          |+ field "x" x (fun x -> x.x)
          |> sealr)]}
*)

val mu2: ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t
(** [mu2 f] is the representations [r] and [s] such that [r, s = mu2 r
    s].

    For instance:

    {[
      type r = { foo: int; bar: string list; z: z option }
      and z = { x: int; r: r list }

      (* Build the representation of [r] knowing [z]'s. *)
      let mkr z =
        record "r" (fun foo bar z -> { foo; bar; z })
        |+ field "foo" int (fun t -> t.foo)
        |+ field "bar" (list string) (fun t -> t.bar)
        |+ field "z" (option z) (fun t -> t.z)
        |> sealr

      (* And the representation of [z] knowing [r]'s. *)
      let mkz r =
        record "z" (fun x r -> { x; r })
        |+ field "x" int (fun t -> t.x)
        |+ field "r" (list r) (fun t -> t.r)
        |> sealr

      (* Tie the loop. *)
      let r, z = mu2 (fun r z -> mkr z, mkz y)]}
*)


(** {1:generics Generic Operations}

    Given a value ['a t], it is possible to define generic operations
    on value of type ['a] such as pretty-printing, parsing and
    unparsing.
*)

val pp: 'a t -> 'a Fmt.t
(** [pp t] is the pretty-printer for values of type [t]. *)

val equal: 'a t -> 'a -> 'a -> bool
(** [equal t] is the equality function between values of type [t]. *)

val compare: 'a t -> 'a -> 'a -> int
(** [compare t] compares values of type [t]. *)

type buffer = Cstruct.t
(** The type for buffers. *)

(** {2 Binary serialization} *)

val size_of: 'a t -> 'a -> int
(** [size_of t] is the size needed to serialize values of type [t]. *)

val write: 'a t -> buffer -> pos:int -> 'a -> int
(** [write t] serializes values of type [t]. Use [size_of] to
    pre-determine the size of the buffer. *)

val read: 'a t ->  buffer -> pos:int -> int * 'a
(** [read t] reads a serialization of a value of type [t]. *)

(** {2 JSON converters} *)

val encode_json: 'a t -> Jsonm.encoder -> 'a -> unit
(** [encode_json e t] encodes [t] into the
    {{:jsonm}http://erratique.ch/software/jsonm}JSON} encoder [e]. *)

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
