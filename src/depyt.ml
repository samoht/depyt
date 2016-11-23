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
  rfields: 'a fields;
}

and 'a fields =
  | F1:
      ('a, 'b) field *
      ('b -> 'a) -> 'a fields
  | F2:
      ('a, 'b) field *
      ('a, 'c) field *
      ('b -> 'c -> 'a) -> 'a fields
  | F3:
      ('a, 'b) field *
      ('a, 'c) field *
      ('a, 'd) field *
      ('b -> 'c -> 'd -> 'a) -> 'a fields
  | F4:
      ('a, 'b) field *
      ('a, 'c) field *
      ('a, 'd) field *
      ('a, 'e) field *
      ('b -> 'c -> 'd -> 'e -> 'a) -> 'a fields
  | F5:
      ('a, 'b) field *
      ('a, 'c) field *
      ('a, 'd) field *
      ('a, 'e) field *
      ('a, 'f) field *
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'a fields
  | F6:
      ('a, 'b) field *
      ('a, 'c) field *
      ('a, 'd) field *
      ('a, 'e) field *
      ('a, 'f) field *
      ('a, 'g) field *
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) -> 'a fields

and ('a, 'b) field = {
  fname: string;
  ftype: 'b t;
  fget : 'a -> 'b;
}

and 'a variant = {
  vwit  : 'a Witness.t;
  vname : string;
  vcases: 'a case array;
  vget  : 'a -> 'a case0;
}

and 'a case =
  | C0: 'a case0 -> 'a case
  | C1: ('a, 'b) case1 -> 'a case

and 'a case0 = {
  ctag0 : int ref;
  cname0: string;
  c0    : 'a;
  cargs : arg list;
}

and arg = Arg: 'a t * 'a -> arg

and ('a, 'b) case1 = {
  ctag1 : int ref;
  cname1: string;
  ctype1: 'b t;
  c1    : 'b -> 'a;
}

type _ a_field = Field: ('a, 'b) field -> 'a a_field

let unit = Prim Unit
let int = Prim Int
let string = Prim String

let list l = List l
let pair a b = Pair (a, b)
let option a = Option a

(* records *)

let field fname ftype fget = { fname; ftype; fget }

let record1 rname a b =
  let rwit = Witness.make () in
  Record { rwit; rname; rfields = F1 (a, b) }

let record2 rname a b c =
  let rwit = Witness.make () in
  Record { rwit; rname; rfields = F2 (a, b, c) }

let record3 rname a b c d =
    let rwit = Witness.make () in
    Record { rwit; rname; rfields = F3 (a, b, c, d) }

let record4 rname a b c d e =
  let rwit = Witness.make () in
  Record { rwit; rname; rfields = F4 (a, b, c, d, e) }

let record5 rname a b c d e f =
  let rwit = Witness.make () in
  Record { rwit; rname; rfields = F5 (a, b, c, d, e, f) }

let record6 rname a b c d e f g =
  let rwit = Witness.make () in
  Record { rwit; rname; rfields = F6 (a, b, c, d, e, f, g) }

(* variants *)

let case0 cname0 c0 =
  let ctag0 = ref (-1) in
  let x = { ctag0; cname0; c0; cargs = [] } in
  C0 x, x

let app1:('a, 'b) case1 -> 'b t -> 'b -> 'a case0 = fun c bt b ->
  let c0 = c.c1 b in
  { ctag0 = c.ctag1; cname0 = c.cname1; c0; cargs = [ Arg (bt, b) ] }

let case1 cname1 ctype1 c1 =
  let ctag1 = ref (-1) in
  let x = { ctag1; cname1; ctype1; c1 } in C1 x, fun y -> app1 x ctype1 y

let variant vname vcases vget =
  let vwit = Witness.make () in
  let set n t i =
    if !t = -1 then t := i
    else if !t = i then ()
    else failwith (n ^ " is already used in another variant at \
                        a different position.")
  in
  List.iteri (fun i -> function
      | C0 { ctag0; cname0; _ } -> set cname0 ctag0 i
      | C1 { ctag1; cname1; _ } -> set cname1 ctag1 i
    ) vcases;
  let vcases = Array.of_list vcases in
  Variant { vwit; vname; vcases; vget }

let enum name l =
  let constr, mk =
    List.fold_left (fun (constr, mk) (n, v) ->
        let c, c0 = case0 n v in
        c :: constr, (v, c0) :: mk
      ) ([], []) (List.rev l)
  in
  variant name constr (fun x -> List.assq x mk)

let fields = function
| F1 (a, _)                -> [Field a]
| F2 (a, b, _)             -> [Field a; Field b]
| F3 (a, b, c, _)          -> [Field a; Field b; Field c]
| F4 (a, b, c, d, _)       -> [Field a; Field b; Field c; Field d]
| F5 (a, b, c, d, e, _)    -> [Field a; Field b; Field c; Field d; Field e]
| F6 (a, b, c, d, e, f, _) -> [Field a; Field b; Field c; Field d; Field e;
                               Field f]

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
    List.for_all (function Field f -> field f x y) (fields r.rfields)

  and field: type a  b. (a, b) field -> a equal = fun f x y ->
    t f.ftype (f.fget x) (f.fget y)

  and variant: type a. a variant -> a equal = fun v x y ->
    case0 (v.vget x) (v.vget y)

  and case0: type a. a case0 equal = fun x y ->
    int x.ctag0.contents y.ctag0.contents && list arg x.cargs y.cargs

  and arg: arg equal = fun (Arg (tx, x)) (Arg (ty, y)) ->
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
    aux (fields r.rfields)

  and field: type a  b. (a, b) field -> a compare = fun f x y ->
    t f.ftype (f.fget x) (f.fget y)

  and variant: type a. a variant -> a compare = fun v x y ->
    case0 (v.vget x) (v.vget y)

  and case0: type a. a case0 compare = fun x y ->
    match int x.ctag0.contents y.ctag0.contents with
    | 0 -> list arg x.cargs y.cargs
    | i -> i

  and arg: arg compare = fun (Arg (tx, x)) (Arg (ty, y)) ->
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
    match r.rfields with
    | F1 (a, _)       ->
        Fmt.pf ppf "@[{ %s = %a }@]" a.fname (field a) x
    | F2 (a, b, _)    ->
        Fmt.pf ppf "@[{ %s = %a; %s = %a }@]"
          a.fname (field a) x b.fname (field b) x
    | F3 (a, b, c, _) ->
        Fmt.pf ppf "@[{ %s = %a; %s = %a; %s = %a }@]"
          a.fname (field a) x b.fname (field b) x c.fname (field c) x
    | F4 (a, b, c, d, _) ->
        Fmt.pf ppf "@[{ %s = %a; %s = %a; %s = %a; %s = %a }@]"
          a.fname (field a) x b.fname (field b) x c.fname (field c) x
          d.fname (field d) x
    | F5 (a, b, c, d, e, _) ->
        Fmt.pf ppf "@[{ %s = %a; %s = %a; %s = %a; %s = %a; %s = %a }@]"
          a.fname (field a) x b.fname (field b) x c.fname (field c) x
          d.fname (field d) x e.fname (field e) x
    | F6 (a, b, c, d, e, f, _) ->
        Fmt.pf ppf "@[{ %s = %a; %s = %a; %s = %a; %s = %a; %s = %a; %s = %a }@]"
          a.fname (field a) x b.fname (field b) x c.fname (field c) x
          d.fname (field d) x e.fname (field e) x f.fname (field f) x

  and field: type a b. (a, b) field -> a Fmt.t = fun f ppf x ->
    t f.ftype ppf (f.fget x)

  and variant: type a. a variant -> a Fmt.t = fun v ppf x ->
    case0 ppf (v.vget x)

  and case0: type a. a case0 Fmt.t = fun ppf c ->
    match c.cargs with
    | [] -> Fmt.string ppf c.cname0
    | l  ->
      Fmt.pf ppf "@[<2>%s %a@]" c.cname0 Fmt.(list ~sep:(unit ",@ ") arg) l

  and arg ppf (Arg (tx, x)) = t tx ppf x

end

let pp = Pp.t

type 'a size_of = 'a -> int

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
    match r.rfields with
    | F1 (a, _)                -> field a x
    | F2 (a, b, _)             -> field a x + field b x
    | F3 (a, b, c, _)          -> field a x + field b x + field c x
    | F4 (a, b, c, d, _)       -> field a x + field b x + field c x + field d x
    | F5 (a, b, c, d, e, _)    -> field a x + field b x + field c x + field d x
                                  + field e x
    | F6 (a, b, c, d, e, f, _) -> field a x + field b x + field c x + field d x
                                  + field e x + field f x

  and field: type a b. (a, b) field -> a size_of = fun f x ->
    t f.ftype (f.fget x)

  and variant: type a. a variant -> a size_of = fun v x ->
    List.fold_left
      (fun acc (Arg (ta, a)) -> acc + t ta a) (int8 0)
      (v.vget x).cargs

end

let size_of = Size_of.t

type 'a write = 'a -> Cstruct.t -> Cstruct.t

module Write = struct

  let unit () buf = buf

  let int8 i buf =
    Cstruct.set_uint8 buf 0 i;
    Cstruct.shift buf 1

  let int i buf =
    Cstruct.BE.set_uint64 buf 0 (Int64.of_int i);
    Cstruct.shift buf 8

  let string str buf =
    let len = String.length str in
    let buf = int len buf in
    Cstruct.blit_from_string str 0 buf 0 len;
    Cstruct.shift buf len

  let list l x buf =
    let buf = int (List.length x) buf in
    List.fold_left (fun buf i -> l i buf) buf x

  let pair a b (x, y) buf = buf |> a x |> b y

  let bool = function
    | false -> int8 0
    | true  -> int8 1

  let option o x buf = match x with
    | None   -> buf |> bool false
    | Some x -> buf |> bool true |> o x

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

  and record: type a. a record -> a write = fun r x buf ->
    match r.rfields with
    | F1 (a, _)                -> buf |> field a x
    | F2 (a, b, _)             -> buf |> field a x |> field b x
    | F3 (a, b, c, _)          -> buf |> field a x |> field b x |> field c x
    | F4 (a, b, c, d, _)       -> buf |> field a x |> field b x |> field c x |>
                                  field d x
    | F5 (a, b, c, d, e, _)    -> buf |> field a x |> field b x  |> field c x |>
                                  field d x |> field e x
    | F6 (a, b, c, d, e, f, _) -> buf |> field a x |> field b x |> field c x |>
                                  field d x |> field e x |> field f x

  and field: type a b. (a, b) field -> a write = fun f x buf ->
    t f.ftype (f.fget x) buf

  and variant: type a. a variant -> a write = fun v x ->
    case0 (v.vget x)

  and case0: type a. a case0 write = fun c buf ->
    buf
    |> int8 c.ctag0.contents (* FIXME: we support 'only' 256 cases *)
    |> List.fold_right (fun (Arg (ta, a)) -> t ta a) c.cargs

end

let write = Write.t

type 'a read = Mstruct.t -> ('a, string) result

module Read = struct

  let (>>=) x f = match x with Ok x -> f x | Error _ as e -> e
  let (>|=) x f = match x with Ok x -> Ok (f x) | Error _ as e -> e

  let unit _ = Ok ()
  let int8 t = Ok (Mstruct.get_uint8 t)
  let int t = Ok (Int64.to_int @@ Mstruct.get_be_uint64 t)
  let string t = int t >|= Mstruct.get_string t

  let list l t =
    int t >>= fun len ->
    let rec aux acc = function
      | 0 -> Ok (List.rev acc)
      | n ->
        match l t with
        | Ok x         ->
          aux (x :: acc) (n - 1)
        | Error _ as e -> e
    in
    aux [] len

  let pair a b t = a t >>= fun a -> b t >|= fun b -> a, b

  let option o t =
    int8 t >>= function
    | 0 -> Ok None
    | _ -> o t >|= fun x -> Some x

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

  and record: type a. a record -> a read = fun r buf ->
    match r.rfields with
    | F1 (a, f) ->
        field a buf >|= fun a ->
        f a
    | F2 (a, b, f) ->
        field a buf >>= fun a ->
        field b buf >|= fun b ->
        f a b
    | F3 (a, b, c, f) ->
      field a buf >>= fun a ->
      field b buf >>= fun b ->
      field c buf >|= fun c ->
      f a b c
    | F4 (a, b, c, d, f) ->
      field a buf >>= fun a ->
      field b buf >>= fun b ->
      field c buf >>= fun c ->
      field d buf >|= fun d ->
      f a b c d
    | F5 (a, b, c, d, e, f) ->
      field a buf >>= fun a ->
      field b buf >>= fun b ->
      field c buf >>= fun c ->
      field d buf >>= fun d ->
      field e buf >|= fun e ->
      f a b c d e
    | F6 (a, b, c, d, e, x, f) ->
      field a buf >>= fun a ->
      field b buf >>= fun b ->
      field c buf >>= fun c ->
      field d buf >>= fun d ->
      field e buf >>= fun e ->
      field x buf >|= fun x ->
      f a b c d e x

  and field: type a  b. (a, b) field -> b read = fun f buf -> t f.ftype buf

  and variant: type a. a variant -> a read = fun v buf ->
    int8 buf >>= fun i -> (* FIXME: we support 'only' 256 variants *)
    case v.vcases.(i) buf

  and case: type a. a case -> a read = fun c buf ->
    match c with
    | C0 c -> Ok c.c0
    | C1 c -> t c.ctype1 buf >|= c.c1

end

let read = Read.t

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
