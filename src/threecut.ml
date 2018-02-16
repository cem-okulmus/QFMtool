open Base
open Fuzzyset 
open List
open Num

type fuzzy_set_tv = (element * num) list

let med_spec a b = match a,b with 
    | a,b when (min_num a b) >/ ((Int 1) // (Int 2)) -> min_num a b
    | a,b when (max_num a b) </ ((Int 1) // (Int 2)) -> max_num a b
    | _ 					   -> ((Int 1) // (Int 2))

let med_gen list = 
	let inf = fold_left min_num (hd list) list in
	let sup = fold_left max_num (hd list) list in 
	med_spec inf sup

let three_cut_general gamma = function 
	| x when x >=/ ((Int 1) // (Int 2)) +/ ((Int 1) // (Int 2)) */ gamma 	-> (Int 1)
    | x when x <=/ ((Int 1) // (Int 2)) -/ ((Int 1) // (Int 2)) */ gamma 	-> (Int 0) 
	| x 								-> ((Int 1) // (Int 2)) 

let three_cut_zero = function
	| x when x >/ ((Int 1) // (Int 2)) 	-> (Int 1)
	| x when x =/ ((Int 1) // (Int 2))  -> (Int 1) // (Int 2) 
	| x 								-> (Int 0) 

let three_cut_elem gamma x = 
	if (gamma =/ (Int 0)) then three_cut_zero x else three_cut_general gamma x

let three_cut gamma (fset:fuzzy_set):fuzzy_set_tv = 
	map (fun (e,f) -> (e,three_cut_elem gamma f)) fset

(* Methods to get crisp sets from three-valued fuzzy sets *)
let tc_min (fset:fuzzy_set_tv) :crisp_set = 
	rev_map fst $ filter (((=/) (Int 1)) $$ snd) fset

let tc_max_ex (fset:fuzzy_set_tv) :crisp_set = 
	rev_map fst $ filter (((=/) ((Int 1) // (Int 2))) $$ snd) fset

let fuzzy_tc_min gamma f = 
	tc_min $ three_cut gamma f
(* let fuzzy_tc_max gamma f  tc_max @@ three_cut gamma f *)

let subsets list = 
	List.fold_left (fun l a -> l @ (rev_map (fun l -> a::l) l)  ) [[]] list 

let tc_range fset :crisp_set list = 
	rev_map ((@) $ tc_min fset) (subsets (tc_max_ex fset))