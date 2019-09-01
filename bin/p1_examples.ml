open P1_core
(* open List_.List_as_set *)

(* to get a visual indication of runtime *)
let start_stop s f = 
  let t1 = Sys.time () in
  print_string ("Start "^s^" ...");
  let _ = f() in
  let t2 = Sys.time () in
  print_endline ("...stop in "^(string_of_float (t2 -. t1))^" seconds");
  ()
  

(* terminals -------------------------------------------------------- *)

(* for testing FIXME use Tjr_substring.a *)
let a1 (s:substring_) = (
  if s.i_ < s.j_ && s.s_.[s.i_] = '1' then
    [("1",{s with i_ = s.i_ + 1})]
  else
    [])

open P1_core

let a1 : string parser_ = (fun i -> a1 i.ss)

let eps i = [("",i.ss)]


(* example grammar: E -> E E E | "1" | eps -------------------- *)

(* FIXME why eta expand? *)
let rec _E i = 
  check "E" (
    ((_E **> _E **> _E) >> fun (x,(y,z)) -> x+y+z)
    ||| (a1 >> fun _ -> 1)
    ||| (eps >> fun _ -> 0)) i

let f () = "111" |> run_parser _E 
let _ = start_stop "example muv" f |> fun _ -> ()

(*
let _ = assert (
  let result = "111" |> run_parser _E in
  let expected = [
    (* FIXME
    (0, `SS ("111", 0, 3)); 
    (1, `SS ("111", 1, 3)); 
    (2, `SS ("111", 2, 3));
    (3, `SS ("111", 3, 3));
    *)
       ]
  in
  set_equal result expected)
*)

let f () = "1111111" |> run_parser _E 
let _ = start_stop "example b1q" f



(* with memo ---------------------------------------- *)


(* we want to create a new hashtable for each new string that we
   parse, hence unit argument *)
let _E () =
  let tbl = Hashtbl.create 100 in
  let rec _E = 
    (fun i -> 
      check "E" (
        let p = 
          ((_E **> _E **> _E) >> (fun (x,(y,z)) -> x+y+z))
          ||| (a1 >> (fun _ -> 1))
          ||| (eps >> (fun _ -> 0))
        in
        memo tbl hashkey_of_input p) i)
  in
  _E
      
let f () = "111" |> run_parser (_E ())
let _ = start_stop "example 63i" f

(* we can now handle much longer inputs with relatively little
   slowdown; the following takes less than 1s compiled *)
let f () = "1111111111111111111111111111111111111111" |> run_parser (_E ())
let _ = start_stop "example 6my" f



(* with memo and dummy actions, i.e. just parsing -------------------- *)

(* we want to create a new hashtable for each new string that we
   parse, hence unit argument *)
let _E () =
  let tbl = Hashtbl.create 100 in
  let rec _E = 
    (fun i -> 
      check "E" (
        let p = 
          ((_E **> _E **> _E) >> (fun _ -> ()))
          ||| (a1 >> (fun _ -> ()))
          ||| (eps >> (fun _ -> ()))
        in
        memo tbl hashkey_of_input p) i)
  in
  _E
      
let f () = "111" |> run_parser (_E ())
let _ = start_stop "example 1cq" f

(* we can now handle much longer inputs with relatively little
   slowdown; the following takes a small fraction of a second *)
let f () = (String.make 20 '1') |> run_parser (_E ()) |> (fun _ -> ())
let _ = start_stop "example yn7" f

let f () = (String.make 40 '1') |> run_parser (_E ())
let _ = start_stop "example rxi" f

let f () = (String.make 60 '1') |> run_parser (_E ())
let _ = start_stop "example vgx" f




(* Sample output from ./p1_examples.native:

Start example muv ......stop in 0.000389 seconds
Start example b1q ......stop in 0.171137 seconds
Start 
example 63i ......stop in 3.5e-05 seconds
Start example 6my ......stop in 0.648735 seconds
Start example 1cq ......stop in 3.4e-05 seconds
Start example yn7 ......stop in 0.031351 seconds
Start example rxi ......stop in 1.058452 seconds
Start example vgx ......stop in 10.121685 seconds


2016-11-28: 

Start example muv ......stop in 0. seconds
Start example b1q ......stop in 0.172 seconds
Start example 63i ......stop in 0. seconds
Start example 6my ......stop in 0.552 seconds
Start example 1cq ......stop in 0. seconds
Start example yn7 ......stop in 0.024 seconds
Start example rxi ......stop in 0.732 seconds
Start example vgx ......stop in 7.328 seconds

2017-07-26:

$ p1_core $ ./p1_examples.native 
Start example muv ......stop in 0. seconds
Start example b1q ......stop in 0.16 seconds
Start example 63i ......stop in 0. seconds
Start example 6my ......stop in 0.564 seconds
Start example 1cq ......stop in 0. seconds
Start example yn7 ......stop in 0.02 seconds
Start example rxi ......stop in 0.748 seconds
Start example vgx ......stop in 7.128 seconds


*)
