(* 	Following the idea of representing fuzzy sets using  
	to ``representation by levels'' taken from Sanchez et al. 2011

	This is essentially a proper generalisation of alpha cuts. 
*)
open List
open Num
open Base
open Fuzzyset
open Cripset

type level			=	num 
type level_mapping  = 	level -> crisp_set
type level_rep 		= 	(level list) * level_mapping

let lmap alpha (lr: level_rep)  : crisp_set = 
	filter ((<=/) alpha) (fst lr)
	|> (hd $$ sort compare_num)
	|> (snd lr) 

let next level_list level = 	
	find ((>/) level ) level_list

let prob_num (levels:level list) val_at_rl y = 
	let lvs =  sort rev_compare ((Int 0)::levels) in
	let unsorted_vals = map snd ( (filter (fun a -> fst a =/ y) val_at_rl)) in 
	let values = sort_uniq rev_compare (unsorted_vals) in
	let l = init (length values) (fun i -> (nth values i) -/ (next lvs $ (nth values i))) in
	reduce (+/) l 

let to_alphacut (fset: fuzzy_set) : level_rep = 
	let levels = sort_uniq compare_num $ (map snd fset)@[Int 1] in 
	let level_mapping a = alpha_cut a fset in 
	(filter ((<>/) (Int 0)) levels), level_mapping

let op operator (ll1,lm1) (ll2,lm2):level_rep = 
	(sort_uniq compare_num (ll1@ll2)),(fun a -> operator (lm1 a) (lm2 a))

let lr_union (lr1:level_rep) (lr2:level_rep) = 
	op union lr1 lr2

let lr_inter (lr1:level_rep) (lr2:level_rep) = 
	op inter lr1 lr2

let lr_diff (lr1:level_rep) (lr2:level_rep) = 
	op diff lr1 lr2