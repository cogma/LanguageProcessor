(* CKY parsing program *)

type terminal = string
type nt = string
type rule_t = nt * terminal  
type rule_nt = nt * nt * nt 
type grammar = rule_t list * rule_nt list

let rec cky_init input tab rules n =
  for i=0 to n-1 do
    let c = input.(i) in
    let nts = List.map fst (List.filter (fun (nt,c')->c=c') rules) in
      tab.(i).(i) <- nts
  done

(* cky fills the table for length>1 *)
let rec cky tab rules n =
  for len=1 to n-1 do 
    for start=0 to (n-1-len) do 
      for mid=start to start+len-1 do
        fill_entry tab rules start mid (start+len)
      done
    done
  done

(* fill_entry fills the entry (start, last) *)
and fill_entry tab rules start mid last =
  let nts1 = tab.(start).(mid) in
  let nts2 = tab.(mid+1).(last) in
    List.iter 
    (fun (nt,nt1,nt2) ->  (* for each rule nt->nt1 nt2 *)
       if List.mem nt1 nts1 && List.mem nt2 nts2 
       then tab.(start).(last) <- nt::(tab.(start).(last))
       else ()
     ) rules

let rec init_tab n =
  let tab = Array.make n (Array.make 0 []) in
   for i=0 to n-1 do
    tab.(i) <- Array.make n []
   done;
   tab

let main (input:terminal array) (g: grammar) =
  let n = Array.length input in
  let tab = init_tab n in
    (cky_init input tab (fst g) n;
     cky tab (snd g) n; 
     tab)

(* an example from Wikipedia page "CYK parsing"
   http://en.wikipedia.org/wiki/CYK_algorithm 
 *)
(* sample inputs *)
let s0 = [|"a"; "man"; "eats"; "a"; "fish"; "with"; "a"; "fork"|]
let s1 = [|"this"; "is"; "a"; "pen"|]
let s2 = [|"this"; "pen"; "is"; "black"|]
(* sample grammar *)
let g0 = 
 ([("N", "man");
   ("NP", "she");
   ("NP", "this");
   ("VP", "eats");
  ("V", "eats");
  ("V", "is");
  ("P", "with");
  ("N", "fish");
  ("N", "fork");
  ("N", "pen");
  ("A", "black");
  ("Det", "a");
  ("Det", "this")],
  [("S", "NP", "VP");
   ("VP", "VP", "PP");
   ("VP", "V", "NP");
   ("VP", "V", "A");
   ("PP", "P", "NP");
   ("NP", "Det", "N")])

(*
main s0 g0;;
*)
