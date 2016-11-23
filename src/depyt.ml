(*---------------------------------------------------------------------------
   Copyright (c) 2016 Thomas Gazagnaire. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)


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
  rname  : string;
  rfields: 'a fields;
}

and 'a fields =
  | F1: ('a, 'b) field * ('b -> 'a) -> 'a fields
  | F2: ('a, 'b) field * ('a, 'c) field * ('b -> 'c -> 'a) -> 'a fields
  | F3: ('a, 'b) field *
        ('a, 'c) field *
        ('a, 'd) field *
        ('b -> 'c -> 'd -> 'a) -> 'a fields

and ('a, 'b) field = {
  fname: string;
  ftype: 'b t;
  fget : 'a -> 'b;
}

and 'a variant = {
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
  cargs : dyn list;
}

and dyn = Dyn: 'a t * 'a -> dyn

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

let record1 rname x y     = Record { rname; rfields = F1 (x, y) }
let record2 rname x y z   = Record { rname; rfields = F2 (x, y, z) }
let record3 rname x y z a = Record { rname; rfields = F3 (x, y, z, a) }

(* variants *)

let dyn t x = Dyn (t, x)

let case0 cname0 c0 =
  let ctag0 = ref (-1) in
  let x = { ctag0; cname0; c0; cargs = [] } in
  C0 x, x

let app1:('a, 'b) case1 -> 'b t -> 'b -> 'a case0 = fun c bt b ->
  let c0 = c.c1 b in
  { ctag0 = c.ctag1; cname0 = c.cname1; c0; cargs = [ Dyn (bt, b) ] }

let case1 cname1 ctype1 c1 =
  let ctag1 = ref (-1) in
  let x = { ctag1; cname1; ctype1; c1 } in C1 x, fun y -> app1 x ctype1 y

let variant vname vcases vget =
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
  Variant { vname; vcases; vget }

let fields = function
  | F1 (f, _)       -> [Field f]
  | F2 (f, g, _)    -> [Field f; Field g]
  | F3 (f, g, h, _) -> [Field f; Field g; Field h]

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
    int x.ctag0.contents y.ctag0.contents && list dyn x.cargs y.cargs

  and dyn: dyn equal = fun (Dyn (xt, x)) (Dyn (ty, y)) ->
    (* FIXME: use type (_, _) eq = Eq: ('a, 'a) eq instead *)
    t xt x (Obj.magic y)

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
    | 0 -> list dyn x.cargs y.cargs
    | i -> i

  and dyn: dyn compare = fun (Dyn (xt, x)) (Dyn (yt, y)) ->
    (* FIXME: type Eq *)
    t xt x (Obj.magic y)

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
    | F1 (f, _)       ->
      Fmt.pf ppf "@[{ %s = %a }@]" f.fname (field f) x
    | F2 (f, g, _)    ->
      Fmt.pf ppf "@[{ %s = %a; %s = %a }@]"
        f.fname (field f) x g.fname (field g) x
    | F3 (f, g, h, c) ->
      Fmt.pf ppf "@[{ %s = %a; %s = %a; %s = %a }@]"
        f.fname (field f) x g.fname (field g) x h.fname (field h) x

  and field: type a b. (a, b) field -> a Fmt.t = fun f ppf x ->
    t f.ftype ppf (f.fget x)

  and variant: type a. a variant -> a Fmt.t = fun v ppf x ->
    case0 ppf (v.vget x)

  and case0: type a. a case0 Fmt.t = fun ppf c ->
    match c.cargs with
    | [] -> Fmt.string ppf c.cname0
    | l  ->
      Fmt.pf ppf "@[<2>%s %a@]" c.cname0 Fmt.(list ~sep:(unit ",@ ") dyn) l

  and dyn ppf (Dyn (tx, x)) = t tx ppf x

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
    | F1 (f, _)       -> field f x
    | F2 (f, g, _)    -> field f x + field g x
    | F3 (f, g, h, c) -> field f x + field g x + field h x

  and field: type a b. (a, b) field -> a size_of = fun f x ->
    t f.ftype (f.fget x)

  and variant: type a. a variant -> a size_of = fun v x ->
    case0 (v.vget x) x

  and case0: type a. a case0 -> a size_of = fun c x ->
    List.fold_left (fun acc (Dyn (ta, a)) -> acc + t ta a) (int8 0) c.cargs

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
    | F1 (f, _)       -> buf |> field f x
    | F2 (f, g, _)    -> buf |> field f x |> field g x
    | F3 (f, g, h, c) -> buf |> field f x |> field g x |> field h x

  and field: type a b. (a, b) field -> a write = fun f x buf ->
    t f.ftype (f.fget x) buf

  and variant: type a. a variant -> a write = fun v x ->
    case0 (v.vget x)

  and case0: type a. a case0 write = fun c buf ->
    buf
    |> int8 c.ctag0.contents (* FIXME: we support 'only' 256 cases *)
    |> List.fold_right (fun (Dyn (ta, a)) -> t ta a) c.cargs

end

let write = Write.t

type 'a read = Mstruct.t -> ('a, string) result

module Read = struct

  let (>>=) x f = match x with Ok x -> f x | Error _ as e -> e
  let (>|=) x f = match x with Ok x -> Ok (f x) | Error _ as e -> e

  let unit t = Ok ()
  let int8 t = Ok (Mstruct.get_uint8 t)
  let int t = Ok (Int64.to_int @@ Mstruct.get_be_uint64 t)
  let string t = int t >|= Mstruct.get_string t

  let list xx l t =
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
    | List l     -> list l (t l)
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
    | F1 (f, c)       -> field f buf >|= fun f -> c f
    | F2 (f, g, c)    -> field f buf >>= fun f -> field g buf >|= fun g -> c f g
    | F3 (f, g, h, c) ->
      field f buf >>= fun f ->
      field g buf >>= fun g ->
      field h buf >|= fun h ->
      c f g h

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

(*
type (_, _) eq = Eq: ('a, 'a) eq

let rec eq: 'a t -> 'b t -> ('a, 'b) eq option = fun x y ->
  match x, y with
  | Unit, Unit     -> Some Eq
  | Int , Int      -> Some Eq
  | String, String -> Some Eq
  | List a, List b -> (match eq a b with Some Eq -> Some Eq | None -> None)
  | Pair (a, b), Pair (c, d) ->
    (match eq a c with
     | None   -> None
     | Some _ -> match eq b d with
       | Some _ -> Some Eq
       | None   -> None)
  | Option a, Option b -> (match eq a b with Some _ -> Some Eq | None -> None)
  | Record a, Record b ->
    (match record_eq a b with Some _ -> Some Eq | None -> None)
  | Variant a, Variant b ->
    (match variant_eq a b with Some _ -> Some Eq | None -> None)
  | _ -> None

and record_eq: type a b. a record -> b record -> (a, b) eq option = fun x y ->
  if x == y || String.compare x.rtag y.rtag = 0 then Some Eq
  else None

and variant_eq: type a b. a variant -> a variant -> (a, b) eq option = fun x y ->
  if x == y || String.compare x.vtag y.vtag = 0 then Some Eq
  else None
*)


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
