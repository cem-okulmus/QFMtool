open Base
open Num
open List

(* set types  *)
type element = string
type crisp_set = element list
type fuzzy_set = (element * num) list

let fmem (s:fuzzy_set) (e:element) = 
	try List.assoc e s with Not_found -> (Int 0)

let combine f (fset1:fuzzy_set) (fset2:fuzzy_set) :fuzzy_set  = 
	let elements1,_ = split fset1 in 
	let elements2,_ = split fset2 in 
	sort_uniq String.compare (elements1 @ elements2) 
	|> map (fun e -> e,f (fmem fset1 e) (fmem fset2 e))

let fmap f fsetlist = 
    reduce (combine f) fsetlist

let union_f = combine max_num

let inter_f = combine min_num

let impl = combine (fun a b -> min_num (Int 1) (((Int 1) -/ a) +/ b))

let equiv a b = inter_f (impl a b) (impl b a)

let init_fset num a b : fuzzy_set = 
	init num (fun x -> string_of_int x, of_float $ a +. ((b -. a) *.(Random.float 1.0)))

let init_fset_const num a : fuzzy_set = 
	init num (fun x -> string_of_int x, max_num (Int 0) ( min_num (Int 1) (Int a)))