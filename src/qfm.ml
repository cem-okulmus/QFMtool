open List
open Num
open Base
open Fuzzyset
open Cripset
open Threecut
open Replevels
open Sfquantifier

(* QFMs *)  
type qfm = (sf_quantor -> ff_quantor)
let qfm_list : (string * qfm) list ref =  ref []

let quantor_tc (quantor:sf_quantor) (fsetlist: fuzzy_set_tv list) = 
    let ranges = rev_map tc_range fsetlist in 
    rev_map quantor.q $ cartesian ranges

let quantor_ff quantor fsets gamma = 
    med_gen $$ quantor_tc quantor $ rev_map (three_cut gamma) fsets

let check x =
    if (x =/ Int 1) || (x =/ 1 /// 2) || (x =/ Int 0) then x 
    else x +/ (Int 1 // Int 10000000)

let weight_tv sorted_values i = 
    (check (nth sorted_values i) ) -/ (check (nth sorted_values (i + 1)))

let weight sorted_values i = 
    (nth sorted_values i)  -/ (nth sorted_values (i + 1))

let get_values (fs:fuzzy_set) = 
    fold_left (fun a (e,f) -> f::a ) [] fs

let finite_sampler fsetlist quantor = 
    let values = sort_uniq rev_compare $ (Int 1)::((Int 0)::(get_values (concat fsetlist))) in
    let m i = weight values i in
    init (length values - 1) (fun i -> (quantor (nth values i)) */ m i) 
    |> reduce (+/)

let get_val_tv (fs:fuzzy_set) =     
    let check x =
        if (x =/ Int 1) || (x =/ 1 /// 2) || (x =/ Int 0) then x 
        else (abs_num $ (x -/ 1 /// 2) */ Int 2) -/ (Int 1 // Int 10000000) in
    fold_left (fun a (e,f) -> (check f)::a ) [] fs 

let finite_sampler_tv fsetlist quantor = 
    let values  = sort_uniq rev_compare $ (Int 1)::((Int 0)::(get_val_tv (concat fsetlist))) in
    let m i = weight_tv values i in
    init (length values - 1) (fun i -> (quantor (nth values i)) */ m i) 
    |> reduce (+/)

(* First DFS presented by Gloeckner *)

let dfs_M quantor fsetlist = 
    (finite_sampler_tv fsetlist) $ quantor_ff quantor fsetlist 

(* B'_cx ( f ) =  sup{min(x,f(x))| x \in I}   *)

let cx_sampler_finite fsetlist f = 
    let values = sort_uniq rev_compare $ (Int 1)::((Int 0)::(get_val_tv (concat fsetlist))) in
    init (length values - 1) (fun i -> min_num (check (nth values i)) (f (nth values i)) ) 
    |> reduce max_num

(* Second DFS described by Gloeckner, has some interesting properties  *)

let dfs_Mcx quantor fsetlist = 
    cx_sampler_finite fsetlist $ quantor_ff quantor fsetlist 

(* Third DFS described by Gloeckner, generalizes OWA  *)

let quantor_top q fsets gamma = 
    reduce max_num $$ quantor_tc q $ rev_map (three_cut gamma) fsets 

let quantor_bot q fsets gamma = 
    reduce min_num $$ quantor_tc q $ rev_map (three_cut gamma) fsets 

let dfs_Fowa q fsets = 
    (fun a -> ( 1 /// 2 */ quantor_top q fsets a) +/ ( 1 /// 2 */ quantor_bot q fsets a))
    |> (finite_sampler_tv fsets)


(* QFMS described by Dı́az-Hermida et al. using probalistic methods *)

(* Maximum Dependence model, assuming that P(alpha) = 1 for 0 <= alpha <= 1 *)

let qfm_FMD (q:sf_quantor)  (fsetlist: fuzzy_set list) =    
    (finite_sampler  fsetlist) (fun alpha -> q.q $ List.rev (rev_map (alpha_cut alpha) fsetlist)) 


(* Independence model, fixing P to 1, but allowing different alpha cuts *)

let qfm_FI q = 
    let rec qfm_FI  ?partial_sets:(p=[]) q fsetlist = match fsetlist with
        | []    -> q.q $ rev p
        | x::xs -> (finite_sampler fsetlist) $ (fun alpha -> 
                        qfm_FI q xs ~partial_sets:((alpha_cut alpha x)::p)) in
    qfm_FI q

(* Approximate Dependence model *)

let qfm_FAD_tau ?tau:(t=1 /// 2) q fsets =  
    (qfm_FMD q fsets) */ t  +/ (qfm_FI q fsets)*/((Int 1) -/ t) 

let qfm_FAD q = qfm_FAD_tau q


(* Probabilistic model *)

(* probability of a crisp set being a representative of a fuzzy one *)
let prob d fset cset = 
    ( fold_left ( */ ) (Int 1) $  map (fmem fset) cset )  */
    ( fold_left ( */ ) (Int 1) $  map (fun a -> Int 1 -/ fmem fset a) (diff d cset))

(* unrestricted in the sense that it takes arbitrary arities *)
let qfm_FA_unrestricted (q:sf_quantor) fsets = 
    let d = alpha_cut (Int 0) (nth fsets 0) in
    cartesian (map (fun a -> subsets d) fsets) 
    |> rev_map (fun y -> (reduce ( */ ) $ rev_map2 (prob d) fsets y) */ q.q y)
    |> reduce (+/) 

(* probability of crisp sets with fixed cardinality being a representative *)
let prob_card d fset j = 
    let rec prob_card ord fset i j =    
        if (i == 0) then (Int 1)
        else
        match j with 
        | 0 ->  
            (prob_card ord fset (i-1) 0) */ ((Int 1) -/ fmem fset (ord i))
        | _ when 1 <= j && j <= (i-1) -> 
            (prob_card ord fset (i-1) j) */ ((Int 1) -/ fmem fset (ord i)) +/
            (prob_card ord fset (i-1) (j-1)) */ (fmem fset (ord i))
        | _ -> 
            (prob_card ord fset (i-1) (i-1)) */ (fmem fset (ord i)) in
    prob_card (fun a -> nth d (a-1)) fset (length d) j 


let qfm_FA_unary  (q:sf_quantor) fsets = 
    if (length fsets <> 1) then 
        failure $ argument_error 1 (length fsets)
    else 
        let d = alpha_cut (Int 0) (nth fsets 0) in 
        let m = length d in
        let cardinalities = BatList.range 0 `To m in 
        reduce (+/) $ map (fun a -> (prob_card d (nth fsets 0) a) */
            (q.q [(BatList.take a d)])) cardinalities


(* Following the summation of rep. levels according to Sanchez et al. 2011 *)
let qfm_SRL (q:sf_quantor) fsets = 
    let unique l = sort_uniq compare_num l in
    let fsets_as_lr = map to_alphacut fsets in 
    let merged_levels = unique $$ flatten $$ fst $ split fsets_as_lr in 
    let values = map (fun a -> q.q $ map (lmap a) fsets_as_lr, a) merged_levels in 
    let prob y = prob_num merged_levels values y in 
    reduce (+/) $ map (fun y -> prob y */ y) (unique $$ fst $ split values)

(* Trying to model QFM from "From SF to FF" from Baldi and Fermüller *)

(* Needed to have both Top and other domain elements in same type *)
type element_with_threshhold = 
    | Element of string 
    | Top 

let from_element e = Element(e)

let prec f c = match c with 
    | Element e -> 
        let c' = fmem f e in
        map (fun (e,f) -> if (f </ c') then e,Int 0 else e, Int 1) f
    | Top       ->
        map (fun (e,f) -> if (f </ Int 1) then e,Int 0 else e, Int 1) f

let alpha_cut_c e (fset:fuzzy_set) : crisp_set = match e with 
    | Element e ->  alpha_cut (fmem fset e) fset
    | Top       ->  alpha_cut (Int 1) fset

let prop_m d f = 
    (reduce (+/) $ rev_map (fmem f) d) // (Int (length d))

let qfm_CL_abstract (q:sf_quantor) fset f = 
    let d = alpha_cut (Int 0) fset in
    let d' = Top::(map from_element d) in
    let ff_quant c = (prop_m d (f (prec fset c) fset)) & (q.q [(alpha_cut_c c fset)])  in 
    reduce max_num $ map ff_quant d'

let qfm_CL_unary (q:sf_quantor) fset = 
    qfm_CL_abstract q fset (fun a b -> equiv a b)

let qfm_CL_unary_inc (q:sf_quantor) fset = 
    qfm_CL_abstract q fset (fun a b -> impl a b)

let qfm_CL_unary_dec (q:sf_quantor) fset = 
    qfm_CL_abstract q fset (fun a b -> impl b a) 

(* Cl generalized to arbitrary arities  *)

let qfm_CL_gen_abstract (q:sf_quantor) fsetlist  gen_prop_m =  
    let d = alpha_cut (Int 0) (nth fsetlist 0) in
    let d' = Top::(map from_element d) in
    let ff_quant elist = (gen_prop_m d fsetlist elist ) & (* <-- Łukasiewicz t-norm *)
        (q.q (map2 (fun a c -> alpha_cut_c c a) fsetlist elist))  in 
    reduce max_num $ map ff_quant (cartesian (map (fun a -> d') fsetlist))

let mon monlist i = 
    if (i >= length monlist) 
        then equiv
    else match nth monlist i with 
    | Non    -> equiv
    | Inc    -> fun a b -> impl a b
    | Dec    -> fun a b -> impl b a

let gen_prop_m_external tnorm monlist d fsetlist elist  = (* generalized prop_m  *)
    let f = mon monlist in 
    reduce tnorm $ BatList.map2i (fun i a v -> prop_m d ((f i) (prec a v) a)) fsetlist elist

let gen_prop_m_internal tnorm monlist d fsetlist elist  = (* generalized prop_m  *)
    let f = mon monlist in 
    prop_m d (fmap tnorm (BatList.map2i (fun i a v -> ((f i) (prec a v) a) ) fsetlist elist))

(* Version 1: generalize prop_m, by externally applying min over all possible arguments*)

let gen_prop_m_min = gen_prop_m_external min_num 

let qfm_CL_min (q:sf_quantor) fsetlist =
    qfm_CL_gen_abstract q fsetlist (gen_prop_m_min q.mon)

(* Version 2: generalize prop_m, by externally applying & over all possible arguments*)
let gen_prop_m_luk = gen_prop_m_external (&)

let qfm_CL_luk (q:sf_quantor) fsetlist =
    qfm_CL_gen_abstract q fsetlist (gen_prop_m_luk q.mon)

(* Version 3: generalize prop_m, by externally applying prod tnorm over all possible arguments *)
let gen_prop_m_prod = gen_prop_m_external ( */ )

let qfm_CL_prod (q:sf_quantor) fsetlist =
    qfm_CL_gen_abstract q fsetlist (gen_prop_m_prod q.mon)


(* Version 4: averaging all arguments by min into one fuzzy set, and applying prop_m *)
let gen_prop_m_internal_min = gen_prop_m_internal min_num

let qfm_CL_min_internal (q:sf_quantor) fsetlist =
    qfm_CL_gen_abstract q fsetlist (gen_prop_m_internal_min  q.mon) 

(* Version 5: averaging all arguments by & into one fuzzy set, and applying prop_m *)
let gen_prop_m_internal_luk  = gen_prop_m_internal  (&)

let qfm_CL_luk_internal (q:sf_quantor) fsetlist =
    qfm_CL_gen_abstract q fsetlist (gen_prop_m_internal_luk q.mon) 

(* Version 6: averaging all arguments by prod tnorm into one fuzzy set, and applying prop_m  *)
let gen_prop_m_internal_prod  = gen_prop_m_internal  ( */ )

let qfm_CL_prod_internal (q:sf_quantor) fsetlist =
    qfm_CL_gen_abstract q fsetlist (gen_prop_m_internal_prod  q.mon) 

(** Register qfms with a string for the tool *)
let add_names () = 
    add qfm_list ("M",dfs_M);
    add qfm_list ("Mcx",dfs_Mcx);
    add qfm_list ("Fowa",dfs_Fowa);
    add qfm_list ("FMD",qfm_FMD);
    add qfm_list ("FI",qfm_FI);
    add qfm_list ("FA",qfm_FA_unrestricted); 
    add qfm_list ("FAD",qfm_FAD);
    add qfm_list ("S",qfm_SRL);
    add qfm_list ("Cl_min",qfm_CL_min);
    add qfm_list ("Cl_luk",qfm_CL_luk);
    add qfm_list ("Cl_prod",qfm_CL_prod);
    add qfm_list ("Cl_min_internal",qfm_CL_min_internal);
    add qfm_list ("Cl_luk_internal",qfm_CL_luk_internal);
    add qfm_list ("Cl_prod_internal",qfm_CL_prod_internal)