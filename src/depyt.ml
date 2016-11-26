(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

type (_, _) eq = Refl: ('a, 'a) eq

module Witness : sig
  type 'a t
  val make : unit -> 'a t
  val eq : 'a t -> 'b t -> ('a, 'b) eq option
end = struct

  type _ equality = ..

  module type Inst = sig
    type t
    type _ equality += Eq : t equality
  end

  type 'a t = (module Inst with type t = 'a)

  let make: type a. unit -> a t = fun () ->
    let module Inst = struct
      type t = a
      type _ equality += Eq : t equality
    end
    in
    (module Inst)

  let eq: type a b. a t -> b t -> (a, b) eq option =
    fun (module A) (module B) ->
      match A.Eq with
      | B.Eq -> Some Refl
      | _    -> None

end

(*
module Hlist = struct

  type _ t =
    | []  : 'a t
    | (::): 'a * 'b t -> ('a * 'b) t

  let cons x y = x :: y
  let empty = []
end
*)

type _ t =
  | Prim   : 'a prim -> 'a t
  | List   : 'a t -> 'a list t
  | Pair   : 'a t * 'b t -> ('a * 'b) t
  | Option : 'a t -> 'a option t
  | Record : 'a record -> 'a t
  | Variant: 'a variant -> 'a t

and 'a prim =
  | Unit   : unit prim
  | Int    : int prim
  | String : string prim

and 'a record = {
  rwit   : 'a Witness.t;
  rname  : string;
  rfields: 'a fields_and_constr;
}

and 'a fields_and_constr =
  | Fields: ('a, 'b) fields * 'b -> 'a fields_and_constr

and ('a, 'b) fields =
  | F0: ('a, 'a) fields
  | F1: ('a, 'b) field * ('a, 'c) fields -> ('a, 'b -> 'c) fields

and ('a, 'b) field = {
  fname: string;
  ftype: 'b t;
  fget : 'a -> 'b;
}

and 'a variant = {
  vwit  : 'a Witness.t;
  vname : string;
  vcases: 'a a_case array;
  vget  : 'a -> 'a case_v;
}

and 'a a_case =
  | C0: 'a case0 -> 'a a_case
  | C1: ('a, 'b) case1 -> 'a a_case

and 'a case_v =
  | CV0: 'a case0 -> 'a case_v
  | CV1: ('a, 'b) case1 * 'b -> 'a case_v

and 'a case0 = {
  ctag0 : int ref;
  cname0: string;
  c0    : 'a;
}

and ('a, 'b) case1 = {
  ctag1 : int ref;
  cname1: string;
  ctype1: 'b t;
  c1    : 'b -> 'a;
}

type _ a_field = Field: ('a, 'b) field -> 'a a_field

(* user-visible types for cases *)
type 'a case = int -> 'a a_case
type 'a case_constr = 'a case_v

let unit = Prim Unit
let int = Prim Int
let string = Prim String

let list l = List l
let pair a b = Pair (a, b)
let option a = Option a

(* records *)

type ('a, 'b, 'c) open_record =
  ('a, 'c) fields -> string * 'b * ('a, 'b) fields

let field fname ftype fget = { fname; ftype; fget }

let record: string -> 'b -> ('a, 'b, 'b) open_record =
  fun n c fs -> n, c, fs

let app: type a b c d.
  (a, b, c -> d) open_record -> (a, c) field -> (a, b, d) open_record
  = fun r f fs ->
    let n, c, fs = r (F1 (f, fs)) in
    n, c, fs

let seal: type a b. (a, b, a) open_record -> a t =
  fun r ->
    let rname, c, fs = r F0 in
    let rwit = Witness.make () in
    Record { rwit; rname; rfields = Fields (fs, c) }

let (|+) = app

(* variants *)

let set_tag n t i =
  if !t = -1 then t := i
  else if !t = i then ()
  else failwith (n ^ " is already used in another variant at \
                      a different position.")

let check_tag n t =
  if !t = -1 then failwith (n ^ " has not been initialized yet")

let case0 cname0 c0 =
  let ctag0 = ref ~-1 in
  let c0 = { ctag0; cname0; c0 } in
  (fun i -> set_tag cname0 ctag0 i; C0 c0), CV0 c0

let case1 cname1 ctype1 c1 =
  let ctag1 = ref ~-1 in
  let c1 = { ctag1; cname1; ctype1; c1 } in
  (fun i -> set_tag cname1 ctag1 i; C1 c1),
  (fun v -> check_tag cname1 ctag1; CV1 (c1, v))

let variant vname vcases vget =
  let vwit = Witness.make () in
  let vcases = List.mapi (fun i c -> c i) vcases in
  let vcases = Array.of_list vcases in
  Variant { vwit; vname; vcases; vget }

let enum vname l =
  let vwit = Witness.make () in
  let _, vcases, mk =
    List.fold_left (fun (i, cases, mk) (n, v) ->
        let c = { ctag0 = ref i; cname0 = n; c0 = v } in
        i+1, (C0 c :: cases), (v, CV0 c) :: mk
      ) (0, [], []) l
  in
  let vcases = Array.of_list (List.rev vcases) in
  Variant { vwit; vname; vcases; vget = fun x -> List.assq x mk }

let rec fields_aux: type a b. (a, b) fields -> a a_field list = function
| F0        -> []
| F1 (h, t) -> Field h :: fields_aux t

let fields r = match r.rfields with
| Fields (f, _) -> fields_aux f

module Refl = struct

  let prim: type a b. a prim -> b prim -> (a, b) eq option = fun a b ->
    match a, b with
    | Unit  , Unit   -> Some Refl
    | Int   , Int    -> Some Refl
    | String, String -> Some Refl
    | _ -> None

  let rec eq: type a b. a t -> b t -> (a, b) eq option = fun a b ->
    match a, b with
    | Prim a, Prim b -> prim a b
    | List a, List b ->
        (match eq a b with Some Refl -> Some Refl | None -> None)
    | Option a, Option b ->
        (match eq a b with Some Refl -> Some Refl | None -> None)
    | Pair (a0, a1), Pair (b0, b1) ->
        (match eq a0 b0, eq a1 b1 with
        | Some Refl, Some Refl -> Some Refl
        | None, _ | _, None -> None)
    | Record a, Record b   -> Witness.eq a.rwit b.rwit
    | Variant a, Variant b -> Witness.eq a.vwit b.vwit
    | _ -> None


end

type 'a equal = 'a -> 'a -> bool

module Equal = struct

  let unit _ _ = true
  let int (x:int) (y:int) = x = y
  let string x y = x == y || String.compare x y = 0

  let list e x y =
    x == y || (List.length x = List.length y && List.for_all2 e x y)

  let pair ex ey (x1, y1 as a) (x2, y2 as b) =
    a == b || (ex x1 x2 && ey y1 y2)

  let option e x y =
    x == y ||
    match x, y with
    | None  , None   -> true
    | Some x, Some y -> e x y
    | _ -> false

  let rec t: type a. a t -> a equal = function
  | Prim p     -> prim p
  | List l     -> list (t l)
  | Pair (x,y) -> pair (t x) (t y)
  | Option x   -> option (t x)
  | Record r   -> record r
  | Variant v  -> variant v

  and prim: type a. a prim -> a equal = function
  | Unit   -> unit
  | Int    -> int
  | String -> string

  and record: type a. a record -> a equal = fun r x y ->
    List.for_all (function Field f -> field f x y) (fields r)

  and field: type a  b. (a, b) field -> a equal = fun f x y ->
    t f.ftype (f.fget x) (f.fget y)

  and variant: type a. a variant -> a equal = fun v x y ->
    case_v (v.vget x) (v.vget y)

  and case_v: type a. a case_v equal = fun x y ->
    match x, y with
    | CV0 x      , CV0 y       -> int x.ctag0.contents y.ctag0.contents
    | CV1 (x, vx), CV1 (y, vy) -> int x.ctag1.contents y.ctag1.contents &&
                                  eq (x.ctype1, vx) (y.ctype1, vy)
    | _ -> false

  and eq: type a b. (a t * a) -> (b t * b) -> bool = fun (tx, x) (ty, y) ->
    match Refl.eq tx ty with
    | Some Refl -> t tx x y
    | None      -> assert false (* this should never happen *)

end

let equal = Equal.t

type 'a compare = 'a -> 'a -> int

module Compare = struct

  let unit (_:unit) (_:unit) = 0
  let int (x:int) (y:int) = Pervasives.compare x y
  let string x y = if x == y then 0 else String.compare x y

  let list c x y =
    if x == y then 0 else
    let rec aux x y = match x, y with
    | [], [] -> 0
    | [], _  -> -1
    | _ , [] -> 1
    | xx::x,yy::y -> match c xx yy with
    | 0 -> aux x y
    | i -> i
    in
    aux x y

  let pair cx cy (x1, y1 as a) (x2, y2 as b) =
    if a == b then 0 else
    match cx x1 x2 with
    | 0 -> cy y1 y2
    | i -> i

  let option c x y =
    if x == y then 0 else
    match x, y with
    | None  , None   -> 0
    | Some _, None   -> 1
    | None  , Some _ -> -1
    | Some x, Some y -> c x y

  let rec t: type a. a t -> a compare = function
  | Prim p     -> prim p
  | List l     -> list (t l)
  | Pair (x,y) -> pair (t x) (t y)
  | Option x   -> option (t x)
  | Record r   -> record r
  | Variant v  -> variant v

  and prim: type a. a prim -> a compare = function
  | Unit   -> unit
  | Int    -> int
  | String -> string

  and record: type a. a record -> a compare = fun r x y ->
    let rec aux = function
    | []           -> 0
    | Field f :: t -> match field f x y with  0 -> aux t | i -> i
    in
    aux (fields r)

  and field: type a  b. (a, b) field -> a compare = fun f x y ->
    t f.ftype (f.fget x) (f.fget y)

  and variant: type a. a variant -> a compare = fun v x y ->
    case_v (v.vget x) (v.vget y)

  and case_v: type a. a case_v compare = fun x y ->
    match x, y with
    | CV0 x      , CV0 y       -> int x.ctag0.contents y.ctag0.contents
    | CV0 x      , CV1 (y, _)  -> int x.ctag0.contents y.ctag1.contents
    | CV1 (x, _) , CV0 y       -> int x.ctag1.contents y.ctag0.contents
    | CV1 (x, vx), CV1 (y, vy) ->
        match int x.ctag1.contents y.ctag1.contents with
        | 0 -> compare (x.ctype1, vx) (y.ctype1, vy)
        | i -> i

  and compare: type a b. (a t * a) -> (b t * b) -> int = fun (tx, x) (ty, y) ->
    match Refl.eq tx ty with
    | Some Refl -> t tx x y
    | None      -> assert false (* this should never happen *)

end

let compare = Compare.t

module Pp = struct

  let unit ppf () = Fmt.string ppf "()"
  let int = Fmt.int
  let string ppf x = Fmt.pf ppf "%S" x
  let list = Fmt.Dump.list
  let pair = Fmt.Dump.pair
  let option = Fmt.Dump.option

  let rec t: type a. a t -> a Fmt.t = function
  | Prim t     -> prim t
  | List l     -> list (t l)
  | Pair (x,y) -> pair (t x) (t y)
  | Option x   -> option (t x)
  | Record r   -> record r
  | Variant v  -> variant v

  and prim: type a. a prim -> a Fmt.t = function
  | Unit   -> unit
  | Int    -> int
  | String -> string

  and record: type a. a record -> a Fmt.t = fun r ppf x ->
    let fields = fields r in
    Fmt.pf ppf "@[{@ ";
    List.iter (fun (Field t) ->
        Fmt.pf ppf "%s = %a;@ " t.fname (field t) x
      ) fields;
    Fmt.pf ppf "}@]"

  and field: type a b. (a, b) field -> a Fmt.t = fun f ppf x ->
    t f.ftype ppf (f.fget x)

  and variant: type a. a variant -> a Fmt.t = fun v ppf x ->
    case_v ppf (v.vget x)

  and case_v: type a. a case_v Fmt.t = fun ppf -> function
  | CV0 x       -> Fmt.string ppf x.cname0
  | CV1 (x, vx) -> Fmt.pf ppf "@[<2>%s %a@]" x.cname1 (t x.ctype1) vx

end

let pp = Pp.t

type buffer = Cstruct.t

type 'a size_of = 'a -> int
type 'a write = buffer -> pos:int -> 'a -> int
type 'a read = buffer -> pos:int -> int * [`Ok of 'a | `Error of string]

module Bin = struct

  module Size_of = struct

    let unit () = 0
    let int8 (_:int) = 1
    let int (_:int) = 8
    let string s = (int 0) + String.length s
    let list l x = List.fold_left (fun acc x -> acc + l x) (int 0) x
    let pair a b (x, y) = a x + b y
    let option o = function
    | None   -> int 0
    | Some x -> (int 0) + o x

    let rec t: type a. a t -> a size_of = function
    | Prim t     -> prim t
    | List l     -> list (t l)
    | Pair (x,y) -> pair (t x) (t y)
    | Option x   -> option (t x)
    | Record r   -> record r
    | Variant v  -> variant v

    and prim: type a. a prim -> a size_of = function
    | Unit   -> unit
    | Int    -> int
    | String -> string

    and record: type a. a record -> a size_of = fun r x ->
      let fields = fields r in
      List.fold_left (fun acc (Field f) -> acc + field f x) 0 fields

    and field: type a b. (a, b) field -> a size_of = fun f x ->
      t f.ftype (f.fget x)

    and variant: type a. a variant -> a size_of = fun v x ->
      match v.vget x with
      | CV0 _       -> (int8 0)
      | CV1 (x, vx) -> (int8 0) + t x.ctype1 vx

  end

  module Write = struct

    let (>>=) = (|>)

    let unit _ ~pos () = pos
    let int8 buf ~pos i = Cstruct.set_uint8 buf pos i; pos+1
    let int buf ~pos i = Cstruct.BE.set_uint64 buf pos (Int64.of_int i); pos+8

    let string buf ~pos str =
      let len = String.length str in
      let pos = int buf ~pos len in
      Cstruct.blit_from_string str 0 buf pos len;
      pos+len

    let list l buf ~pos x =
      let pos = int buf ~pos (List.length x) in
      List.fold_left (fun pos i -> l buf ~pos i) pos x

    let pair a b buf ~pos (x, y) =
      a buf ~pos x >>= fun pos ->
      b buf ~pos y

    let bool buf ~pos = function
    | false -> int8 buf ~pos 0
    | true  -> int8 buf ~pos 1

    let option o buf ~pos = function
    | None   -> bool buf ~pos false
    | Some x -> bool buf ~pos true >>= fun pos -> o buf ~pos x

    let rec t: type a. a t -> a write = function
    | Prim t     -> prim t
    | List l     -> list (t l)
    | Pair (x,y) -> pair (t x) (t y)
    | Option x   -> option (t x)
    | Record r   -> record r
    | Variant v  -> variant v

    and prim: type a. a prim -> a write = function
    | Unit   -> unit
    | Int    -> int
    | String -> string

    and record: type a. a record -> a write = fun r buf ~pos x ->
      let fields = fields r in
      List.fold_left (fun pos (Field f) -> field f buf ~pos x) pos fields

    and field: type a b. (a, b) field -> a write = fun f buf ~pos x ->
      t f.ftype buf ~pos (f.fget x)

    and variant: type a. a variant -> a write = fun v buf ~pos x ->
      case_v buf ~pos (v.vget x)

    and case_v: type a. a case_v write = fun buf ~pos c ->
      match c with
      | CV0 c     -> int8 buf ~pos c.ctag0.contents
      | CV1 (c,v) ->
          int8 buf ~pos c.ctag1.contents >>= fun pos ->
          t c.ctype1 buf ~pos v

  end

  module Read = struct

    let (>|=) (pos, x) f =
      match x with
      | `Ok x    -> pos, `Ok (f x)
      | `Error e -> pos, `Error e

    let (>>=) (pos, x) f =
      match x with
      | `Ok x    -> f (pos, x)
      | `Error e -> pos, `Error e

    type 'a res = int * [`Ok of 'a | `Error of string]

    let unit _ ~pos = pos, `Ok ()
    let int8 buf ~pos = pos+1, `Ok (Cstruct.get_uint8 buf pos)

    let int buf ~pos =
      pos+8, `Ok (Int64.to_int @@ Cstruct.BE.get_uint64 buf pos)

    let string buf ~pos =
      int buf ~pos >>= fun (pos, len) ->
      let str = Bytes.create len in
      Cstruct.blit_to_string buf pos str 0 len;
      pos+len, `Ok (Bytes.unsafe_to_string str)

    let list l buf ~pos =
      int buf ~pos >>= fun (pos, len) ->
      let rec aux acc ~pos = function
      | 0 -> pos, `Ok (List.rev acc)
      | n ->
          l buf ~pos >>= fun (pos, x) ->
          aux (x :: acc) ~pos (n - 1)
      in
      aux [] ~pos len

    let pair: type a b. a read -> b read -> (a * b) read = fun a b buf ~pos ->
      a buf ~pos >>= fun (pos, a) ->
      b buf ~pos >|= fun b ->
      (a, b)

    let option: type a. a read -> a option read = fun o buf ~pos ->
      int8 buf ~pos >>= function
      | pos, 0 -> pos, `Ok None
      | pos, _ -> o buf ~pos >|= fun x -> Some x

    let rec t: type a. a t -> a read = function
    | Prim t     -> prim t
    | List l     -> list (t l)
    | Pair (x,y) -> pair (t x) (t y)
    | Option x   -> option (t x)
    | Record r   -> record r
    | Variant v  -> variant v

    and prim: type a. a prim -> a read = function
    | Unit   -> unit
    | Int    -> int
    | String -> string

    and record: type a. a record -> a read = fun r buf ~pos ->
      match r.rfields with
      | Fields (fs, c) ->
          let rec aux: type b. pos:int -> b -> (a, b) fields -> a res
            = fun ~pos f -> function
            | F0         -> pos, `Ok f
            | F1 (h, t) ->
                field h buf ~pos >>= fun (pos, x) ->
                aux ~pos (f x) t
          in
          aux ~pos c fs

    and field: type a  b. (a, b) field -> b read = fun f -> t f.ftype

    and variant: type a. a variant -> a read = fun v buf ~pos ->
      (* FIXME: we support 'only' 256 variants *)
      int8 buf ~pos >>= fun (pos, i) ->
      case v.vcases.(i) buf ~pos

    and case: type a. a a_case -> a read = fun c buf ~pos ->
      match c with
      | C0 c -> pos, `Ok c.c0
      | C1 c -> t c.ctype1 buf ~pos >|= c.c1

  end

  let size_of = Size_of.t
  let read = Read.t
  let write = Write.t

end

module Json = struct
  module Sizer_of = struct
    let t _ = failwith "TODO"
  end
  module Write = struct
    let t _ = failwith "TODO"
  end
  module Read = struct
    let t _ = failwith "TODO"
  end
  let size_of = Sizer_of.t
  let write = Write.t
  let read = Read.t
end

module type Serializer = sig
  val size_of: 'a t -> 'a -> int
  val write: 'a t -> buffer -> pos:int -> 'a -> int
  val read: 'a t ->  buffer -> pos:int -> int * [`Ok of 'a | `Error of string]
end

let test t = Alcotest.testable (pp t) (equal t)

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
