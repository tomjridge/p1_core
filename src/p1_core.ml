(* NOTE this is less general than it could be: no need to assume we
   are parsing strings *)

open Tjr_monad.Types

(** Dummy type, to represent sequencing and to allow rec bindings with
   monad *)
type xx

(** A parser returns a list of results FIXME but if we have a list of
   results, then we can't just record a single state of the context in
   the monad; thus, for a list of results we also need to explicitly
   return the parse state with each result... alternatively we could
   hide this altogether and instead maintain everything inside the
   monad, ie ('a,'t) m somehow hides the fact that we have a list; the
   bind would then need to be more complicated 

    in this case, we would not deal with list ops explicitly-  they would be hidden by the monad
*)
type ('a,'t) parser_ = xx -> ('a list,'t) m


(* nonterm ---------------------------------------------------------- *)

(* use type var 'nt *)

(* contexts --------------------------------------------------------- *)

module Ctxt_type = struct

  (* ctxt is a list of (nt,i,j) recording that we are already parsing nt
     for span i,j; since in a ctxt all i,j are equal, we can just record a
     list of nts with the span i,j *)
  type 'nt ctxt = { nts: 'nt list; i_:int; j_:int }

  let empty_ctxt = { nts=[]; i_=0; j_=max_int }  (* FIXME max_int a bit ugly *)

end
open Ctxt_type


(* inputs ----------------------------------------------------------- *)

(* we keep the input type abstract, but we assume that there is some
   ctxt obtainable from the monad *)



(* combinators ---------------------------------------- *)

(* It is worth noting that nothing in the following definitions
   depends on the notion of context. Context comes later, and is
   modularly combined with the following. *)

let mk_combs ~monad_ops =
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let seq p1 p2 = 
    fun xx -> 
      p1 xx >>= fun as_ ->
      (* NOTE we may want to allow p2 to be parameterized by the
         result of p1... in fact we likely do; in which case the result is a list of bs *)
      return (List.map (fun a -> p2 a xx) as_)  >>= fun bs -> 
      return (List.concat bs)
  in
  let alt p1 p2 = 
    fun xx ->
      p1 xx >>= fun as_ ->
      p2 xx >>= fun bs ->
      return 
      
    

let ( >> ) p f = fun _ -> 
  
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

