(* NOTE this is less general than it could be: no need to assume we
   are parsing strings *)

(* substrings ------------------------------------------------------- *)

type substring_ = { 
  s_:string;
  i_:int;
  j_:int;
}

let mk_substring s = { s_=s; i_=0; j_=String.length s }

let sublen ss = ss.j_ - ss.i_

let dec_j s = 
  assert (sublen s > 0);
  {s with j_=s.j_ - 1}

let inc_j s = {s with j_=s.j_ + 1}


type ss = substring_



(* nonterm ---------------------------------------------------------- *)

(* FIXME prefer to keep this abstract *)
type nonterm = string


(* contexts --------------------------------------------------------- *)

(* ctxt is a list of (nt,i,j) recording that we are already parsing nt
   for span i,j; since in a ctxt all i,j are equal, we can just record a
   list of nts with the span i,j *)
type ctxt = { nts: nonterm list; span: int * int }

let empty_ctxt = { nts=[]; span=(0,max_int) }  (* FIXME max_int a bit ugly *)



(* inputs ----------------------------------------------------------- *)

type input = { ctxt:ctxt; ss : substring_ }

let to_input ss = { ctxt=empty_ctxt; ss }

let string_to_input s = s |> mk_substring |> to_input

(* FIXME needed? *)
let lift f i = {i with ss=(f i.ss) } 


(* results and parsers ---------------------------------------------- *)

type 'b result = ('b * ss) list

type 'b parser_ = input -> 'b result



(* combinators ---------------------------------------- *)

(* It is worth noting that nothing in the following definitions
   depends on the notion of context. Context comes later, and is
   modularly combined with the following. *)

let unique xs = List.sort_uniq Pervasives.compare xs

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
  | l when l < 0 -> assert false
  | 0 -> []
  | _ -> {i with ss = i.ss|>dec_j} |> p |> List.map (fun (v,s) -> (v,s|>inc_j)))

let (_: 'b parser_ -> 'b parser_) = ignr_last



(* more context ------------------------------------ *)

let update ~ctxt (nt,l,h) = 
  let (l',h') = ctxt.span in
  assert(l>=l' && h <= h');
  match (l,h) = (l',h') with
  | true ->
    assert(not (List.mem nt ctxt.nts));
    { ctxt with nts=nt::ctxt.nts }
  | false ->
    { nts=[nt]; span=(l,h) }

let contains ~ctxt (nt,l,h) = 
  ctxt.span = (l,h) && List.mem nt ctxt.nts


(* remember what NT is called on what input *)
(* nonterm -> 'a parser_ -> 'a parser_ *)
(* was update_lctxt *)
let update_p nt p = fun i0 ->
  p { i0 with ctxt=(update i0.ctxt (nt,i0.ss.i_,i0.ss.j_)) }

let (_:nonterm -> 'b parser_ -> 'b parser_) = update_p

let check nt p = fun i0 ->
  let should_trim = contains i0.ctxt (nt,i0.ss.i_,i0.ss.j_) in
  if should_trim && (i0.ss|>sublen = 0) then
    []
  else if should_trim then
    ignr_last (update_p nt p) i0
  else
    (update_p nt p) i0

let (_:nonterm -> 'b parser_ -> 'b parser_) = check


(* memoization support ---------------------------------------- *)

(* FIXME really this needs to have an gensymed int identifier for the
   string component *)
type hashkey = (ctxt * int * int)

let hashkey_of_input i0 = (i0.ctxt,i0.ss.i_,i0.ss.j_)

(* generic memo function *)
let memo tbl key_of_input f i = 
  key_of_input i |> fun k -> 
  if (Hashtbl.mem tbl k) then 
    Hashtbl.find tbl k
  else (
    let v = f i in
    Hashtbl.add tbl k v;
    v)


(* convenience ---------------------------------------- *)

(* return those results for which entire input is parsed *)
let run_parser : 'b parser_ -> string -> 'b list = 
  fun p s -> 
    s |> mk_substring |> to_input |> p |> 
    List.filter (fun (_,s) -> s|>sublen=0) |> List.map fst

