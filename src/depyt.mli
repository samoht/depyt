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

val mu: ('a t -> 'a t) -> 'a t
(** [mu f] is the representation [r] such that [r = mu r]. *)

val mu2: ('a t -> 'b t -> 'a t * 'b t) -> 'a t * 'b t
(** [mu2 f] is the representations [r] and [s] such that [r, s = mu2 r s]. *)

(** {1 Records} *)

type ('a, 'b) field
(** The type for fields holding values of type ['b] and belonging to a
    record of type ['a]. *)

val field: string -> 'a t -> ('b -> 'a) -> ('b, 'a) field
(** [field n t g] is the representation of the field [n] of type [t]
    with getter [g]. *)

type ('a, 'b, 'c) open_record
(** The type for representing open records of type ['a] with
    constructors of type ['c]. ['a] represents the fields missings to
    the record, e.g. an open record can be {{!seal}sealed} when ['a =
    'c]. *)

val seal: ('a, 'b, 'a) open_record -> 'a t
(** [seal r] seal the open record [r]. *)

val (|+):
  ('a, 'b, 'c -> 'd) open_record -> ('a, 'c) field -> ('a, 'b, 'd) open_record
(** [r |+ f] adds the field [f] to the open record [r]. *)

val record: string -> 'b -> ('a, 'b, 'b) open_record
(** [record n f fs] is the representation of the record called [n] of
    type ['a] using [f] as constructor and with the fields [fs].

    For instance:

    {[
      type t = { foo: string; bar = (int * string) list; }

      let t =
        record "t" (fun foo -> { foo })
        |+ field "foo" string (fun t -> t.foo)
        |+ field "bar" (list (pair int string)) (fun t -> t.bar)
        |> seal
    ]}
*)

(** {1 Variants} *)

type 'a case
(** The type for representing variant cases of type ['a]. *)

type 'a case_constr
(** The type for representing case constructors for the type ['a]. *)

val case0: string -> 'a -> 'a case * 'a case_constr
(** [case0 n v] is a representation of a variant case [n] with no
    argument and a representation of its constructor. e.g.

    {[
      type t = Foo

      let foo = case0 "Foo" Foo
    ]}
*)

val case1: string -> 'b t -> ('b -> 'a) -> 'a case * ('b -> 'a case_constr)
(** [case1 n t c] is a representation of a variant case [n] with 1
    argument of type [t] and constructor [c]. e.g.

    {[
      type t = Foo of string

      let foo = case1 "Foo" string (fun s -> Foo s)
    ]}
*)

val variant: string -> 'a case list -> ('a -> 'a case_constr) -> 'a t

(** [variant n c p] is a representation of a variant type containing
    the cases [c] and using [p] to deconstruct values, e.g.

    {[
      type t = Foo | Bar of string

      let t =
        let foo, mk_foo = case0 "Foo" Foo in
        let bar, mk_bar = case1 "Bar" string (fun x -> Bar x) in
        variant "t" [foo; bar]
        @@ function Foo -> mk_foo | Bar x -> mk_bar x
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

type buffer = Cstruct.t
(** The type for buffers. *)

(** Serialization. *)
module type Serializer = sig

  val size_of: 'a t -> 'a -> int
  (** [size_of t] is the size needed to serialize values of type [t]. *)

  val write: 'a t -> buffer -> pos:int -> 'a -> int
  (** [write t] serializes values of type [t]. Use [size_of] to
      pre-determine the size of the buffer. *)

  val read: 'a t ->  buffer -> pos:int -> int * [`Ok of 'a | `Error of string]
  (** [read t] reads a serialization of a value of type [t]. *)

end

(** Binary serializers. *)
module Bin: Serializer

(** JSON serializers. *)
module Json: Serializer

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
