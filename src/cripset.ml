open List
open Num
open Fuzzyset
open Base 

let cmem  (c:crisp_set)  (e:element)= 
	if mem e c then Int 1 else Int 0

let union (b1:crisp_set) (b2:crisp_set) = 
	sort_uniq String.compare (b1 @ b2)

let inter (b1:crisp_set) (b2:crisp_set) = 
	filter (fun a -> mem a b2) b1

let diff (b1:crisp_set) (b2:crisp_set) = 
	filter (fun a -> not $ mem a b2) b1

let neg base (c:crisp_set) = 
	diff base c

let alpha_cut alpha (fset:fuzzy_set) : crisp_set = 
	map (fun (e,f) -> e) $ filter (fun (e,f) -> f >=/ alpha) fset