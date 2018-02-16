open List
open Num 
open Base
open Cripset
open Fuzzyset
open Sfquantifier
open Qfm

let crisp_list : (string * crisp_set) list ref = ref []
let fuzzy_list : (string * fuzzy_set) list ref =  ref []

exception ObjectNotFound of string * string

let flush_list_buffers () =
    crisp_list := [];
    fuzzy_list := []

let get_qfm name = 
    try 
	   assoc name !qfm_list 
    with Not_found -> raise (ObjectNotFound ("QFM",name))

let get_sfq name = 
    try
	   assoc name !sfq_list
    with Not_found -> raise (ObjectNotFound ("semi-fuzzy quantifier",name))

let get_sfq_giles name a b = 
    try
        assoc name !sfq_list_giles a b
    with Not_found -> raise (ObjectNotFound ("semi-fuzzy quantifier",name))

let get_sfq_unary name a = 
    try
        assoc name !sfq_list_unary a 
    with Not_found -> raise (ObjectNotFound ("semi-fuzzy quantifier",name))

let add_fuzzy name fset = 
    let floor_ceiling n = max_num (Int 0) (min_num (Int 1) n) in
    let fset = map (fun  (e,f) -> e, floor_ceiling f) fset  in
	fuzzy_list := (name,fset)::!fuzzy_list

let add_crisp name cset = 
	crisp_list := (name,cset)::!crisp_list

let get_fuzzy name = 
    try 
	   assoc name !fuzzy_list
    with Not_found -> raise (ObjectNotFound ("fuzzy set",name))

let get_crisp name = 
    try
	   assoc name !crisp_list    
    with Not_found -> raise (ObjectNotFound ("crisp set",name))