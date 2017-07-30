(* NOTE this is less general than it could be: no need to assume we
   are parsing strings *)

open Tjr_substring

let sublen = Tjr_substring.length


let dec_j s = {s with j_=s.j_ - 1}

let inc_j s = {s with j_=s.j_ + 1}


type nonterm = string


module Context' = struct
  (* ctxt is a list of (nt,i,j) recording that we are already parsing nt for span i,j *)
  type elt = (nonterm * int * int)  (* FIXME really need an extra arg int for the string *)
  type ctxt = elt list 
  let empty = []
end
include Context'

(* type 'a s = 'a Substring.t *)
type ss = substring_


type input = { ctxt:ctxt; ss : substring_ }

let to_input ss = { ctxt=empty; ss }

let lift f i = {i with ss=(f i.ss) } 


type 'b result = ('b * ss) list

type 'b parser_ = input -> 'b result



(* combinators ---------------------------------------- *)

(* It is worth noting that nothing in the following definitions
   depends on the notion of context. Context comes later, and is
   modularly combined with the following. *)

let unique = Tjr_list.unique

let ( >> ) p f = (fun i0 ->
    i0 |> p |> List.map (fun (e,s) -> (f e, s)) |> unique)

let (_: 'b parser_ -> ('b -> 'c) -> 'c parser_) = ( >> )

let ( ||| ) p1 p2 = fun s -> List.append (p1 s) (p2 s) |> unique

let (_: 'b parser_ -> 'b parser_ -> 'b parser_) = ( ||| )

(* a version of the combinator that ignores duplicate entries FIXME *)
let ( **> ) p1 p2 i = (
  let f (e1,s1) =
    { ctxt=i.ctxt; ss=s1 } |> p2 |> List.map (fun (e2,s2) -> ((e1,e2),s2))
  in
  i |> p1 |> List.map f |> List.concat)

let (_: 'b parser_ -> 'c parser_ -> ('b*'c) parser_) = ( **> )


let ignr_last p i = (
  match (i.ss|>sublen) with
  | 0 -> []
  | _ -> {i with ss = i.ss|>dec_j} |> p |> List.map (fun (v,s) -> (v,s|>inc_j)))

let (_: 'b parser_ -> 'b parser_) = ignr_last



(* context ---------------------------------------- *)

module Context = struct

  (* debug version; assumes s1 = s2 (since the only part of the
     context that matters is...) *)
  let cmp (nt1,l1,h1) (nt2,l2,h2) = (
    assert ((l1,h1) = (l2,h2)); (* contexts are normalized to a particular span *)
    Pervasives.compare nt1 nt2)

  (* when parsing the input between l and h, the only part of the
     context that matters are those entries (nt,(l',h')) st (l',h') =
     (l,h); so there is a notion of a normalized context (important
     for memoization) *)

  let normalize c (l,h) = (
    c |> List.filter (fun (nt',l',h') -> (l',h') = (l,h)) )

  (* our contexts are sorted; we need insertion into a sorted list; we expect no duplicates  *)
  let rec insert ~cmp elt lst = (
    match lst with
    | [] -> [elt]
    | x::xs -> 
      cmp elt x |> fun r -> 
      match () with
      | _ when r < 0 -> 
        (* elt < x *)
        elt :: lst 
      | _ when r = 0 -> 
        failwith __LOC__ (* no duplicates *)
      | _ -> x :: insert ~cmp elt xs)

  let update c (nt,l,h) = (
    let c' = normalize c (l,h) in
    (insert ~cmp (nt,l,h) c'))

  let contains c (nt,l,h) = (List.mem (nt,l,h) c)


  (* remember what NT is called on what input *)
  (* nonterm -> 'a parser_ -> 'a parser_ *)
  (* was update_lctxt *)
  let update_p nt p = (fun i0 ->
      p { i0 with ctxt=(update i0.ctxt (nt,i0.ss.i_,i0.ss.j_)) })

  let (_:nonterm -> 'b parser_ -> 'b parser_) = update_p

  let check nt p = (fun i0 ->
      let should_trim = contains i0.ctxt (nt,i0.ss.i_,i0.ss.j_) in
      if should_trim && (i0.ss|>sublen = 0) then
        []
      else if should_trim then
        (ignr_last (update_p nt p)) i0
      else
        (update_p nt p) i0)

  let (_:nonterm -> 'b parser_ -> 'b parser_) = check

end
include Context

(* memoization support ---------------------------------------- *)

(* FIXME really this needs to have an gensymed int identifier for the
   string component *)
type hashkey = (ctxt * int * int)

let hashkey_of_input i0 = (i0.ctxt,i0.ss.i_,i0.ss.j_)

(* generic memo function *)
let memo tbl key_of_input f i = (
  key_of_input i |> fun k -> 
  if (Hashtbl.mem tbl k) then 
    Hashtbl.find tbl k
  else
    let v = f i in
    let _ = Hashtbl.add tbl k v in
    v)


(* convenience ---------------------------------------- *)

(* return those results for which entire input is parsed *)
let run_parser : 'b parser_ -> string -> 'b list = (
  fun p s -> 
    s |> mk_substring |> to_input |> p |> 
    List.filter (fun (_,s) -> s|>sublen=0) |> List.map fst)

