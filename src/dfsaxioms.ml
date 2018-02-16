open Num
open List
open Base
open Cripset
open Fuzzyset
open Replevels
open Sfquantifier
open Qfm

let to_fuzzy domain crisp_set =
    map (fun e -> if mem e crisp_set then e,Int 1 else e,Int 0) domain

let to_sf_quantor d  q : ((crisp_set list) -> num) =
    (fun arg -> q (map (to_fuzzy d ) arg) )

let general_chek qfm q d arg =
    ( to_sf_quantor d (qfm q)  arg    ,  q.q arg )

let eta_1 (n: int ) (l:crisp_set) : bool list =
    init n (fun a -> mem (string_of_int (a+1)) l)

let eta (l:bool list) : crisp_set =
    let inital = init (length l) (fun a -> a) in
    filter (fun a -> (nth l a) == true) inital
    |> map string_of_int

let eta_f_1 (f: fuzzy_set ) : num list =
    map (fun (e,f) -> f) f

let eta_f (l: num list) : fuzzy_set =
    init (length l) (fun a -> (string_of_int (a+1), nth l a));;

let b_neg (b: bool list) =
    if (nth b 0) then Int 0 else Int 1

let b_and (b: bool list) =
    if (nth b 0) && (nth b 1) then Int 1 else Int 0

let b_or (b: bool list) =
    if (nth b 0) || (nth b 1) then Int 1 else Int 0

let q_neg  =
    { q = (fun s -> b_neg ( (eta_1  1) (hd s))); mon = []}

let q_and  =
    { q = (fun s -> b_and ( (eta_1  2) (hd s))); mon = []}

let q_or  =
    { q = (fun s -> b_or ((eta_1 2) (hd s))); mon = []}

let induced_neg qfm f  :num= qfm q_neg [eta_f [f]]

let induced_and qfm f1 f2 = qfm q_and [eta_f [f1;f2]]

let induced_or (qfm: sf_quantor -> ff_quantor) f1 f2 = qfm q_or [eta_f [f1;f2]]

let induced_set_unary i fset = map (fun (e,n) -> (e, i n)) fset

let induced_set_binary i fset1 fset2 = combine i fset1 fset2

let induced_quantor (i:num -> num) (q:sf_quantor) arg = i (q.q arg)

let rec change_last f = function
    | []        -> []
    | x::[]     -> [f x]
    | x::xs     ->  x::change_last f xs

let dual_quantor_sf base qfm (q:sf_quantor) =
    { q = (fun arg -> (induced_quantor (induced_neg qfm)) q (change_last (neg base) arg));
    mon = []  }  (* not sure how monotonicity is affected by dual *)

let dual_quantor_ff qfm (q:ff_quantor) (arg: fuzzy_set list) =
    let neg = (induced_neg qfm) in
    neg $ q (change_last (induced_set_unary neg) arg)

let rec change_last_two f = function
    | []        -> []
    | x::(y::[]) -> [f x y]
    | x::xs      ->  x::change_last_two f xs

let union_sf quantor  =
    { q = (fun arg -> quantor.q (change_last_two union arg) );
     mon = quantor.mon} (* no change in monotonicity *)

let union_ff (qfm:sf_quantor -> ff_quantor) (q:ff_quantor) (arg: fuzzy_set list) =
    let i_union = induced_set_binary (induced_or qfm) in
    q (change_last_two i_union arg)


let dual_check qfm base quantor arg =
    (qfm ( dual_quantor_sf base qfm quantor) arg, dual_quantor_ff qfm (qfm quantor) arg)

let union_chek qfm quantor arg =
    ((qfm (union_sf quantor) ) arg, union_ff qfm (qfm quantor) arg )


let induced_ext (qfm:qfm) h (set:fuzzy_set) : fuzzy_set =
    let q e = { q = (fun arg -> (pro_e (h e)).q (map (map h) arg))     ;
                mon =  [];} in
    map (fun (e,f) -> (h e, qfm (q e) [set] )) set

let get_isomorp d1 d2 =
    fun e -> assoc e (map2 (fun a b -> (a,b) ) d1 d2 )

let functional_check (qfm:qfm) quantor h arg =
    let q = { q = (fun arg -> quantor.q (map (map h) arg) );
              mon = quantor.mon;
            } in
    qfm q arg, (qfm quantor) $ map (induced_ext qfm h) arg


let rec error_search f qfm quantor size arg_count iterations =
    if iterations > 0 then
        let arg = init arg_count (fun a -> init_fset size 0. 1.) in
        let base = flatten $ map (fun a -> alpha_cut (Int 0) a) arg in
        let left,right = f qfm base quantor arg in
        if left =/ right then
            error_search f qfm quantor size arg_count (iterations - 1)
        else
            Some (left,right,arg)
    else
        None

let increase  =
    map (map (fun (e,f) -> e, min_num (Int 1) $ f +/ (of_float $ Random.float 1.0)))

let rec monotonicity_check qfm quantor size arg_count iterations  increasing =
    let first = hd $ init (arg_count-1) (fun a -> init_fset_const size 1) in
    let rest = init (arg_count-1) (fun a -> init_fset size 0. 1.) in
    let initial_value = qfm quantor (first::rest) in
    let transformed_input = first::(increase rest) in
    let second_value = qfm quantor transformed_input in
    if increasing && (second_value </ initial_value) then
        Some ((first::rest),transformed_input,increasing,initial_value,second_value)
    else if not increasing && (second_value >/ initial_value) then
        Some ((first::rest),transformed_input,increasing,initial_value,second_value)
    else if iterations > 0 then
        monotonicity_check qfm quantor size arg_count (iterations-1) increasing
    else None


let test_all_qfms q arg arg2 =
    let f (a,b) = string_of_num a ++ " , " ++ string_of_num b in
    let test_qfm (s,qfm) = Printf.printf "QFM: %s, Result: %s \n" s (f (qfm q arg, qfm q arg2)) in
    map test_qfm (rev !qfm_list);;

let young =
 [("Sue", 2 /// 7); ("Marcus", 3 /// 7); ("Michael", 4 /// 7)
 ; ("Mary", 5 /// 7); ("James", 6 /// 7); ("Sarah", Int 1); ("Kathrine", 6 /// 7)
 ; ("Martin", 5 /// 7); ("Lucas", 4 /// 7); ("John", 3 /// 7); ("Patrick", 2 /// 7)]


let dilligent =
 [ ("Sue", 6 /// 7); ("Marcus", 5 /// 7); ("Michael", 4 /// 7)
 ; ("Mary", 3 /// 7); ("James", 2/// 7); ("Sarah", 1 /// 7); ("Kathrine", 2/// 7)
 ; ("Martin", 3 /// 7); ("Lucas", 4 /// 7); ("John", 5 /// 7); ("Patrick", 6 /// 7) ]


let dilligent2 =
 [ ("Sue", Int 1); ("Marcus", 5 /// 7); ("Michael", 4 /// 7)
 ; ("Mary", 3 /// 7); ("James", 3/// 7); ("Sarah", 6 /// 7); ("Kathrine", 4/// 7)
 ; ("Martin", 3 /// 7); ("Lucas", Int 1); ("John",Int 1); ("Patrick", 6 /// 7) ]

let (younge:fuzzy_set) = [("Marcus",3 /// 7);("Mary", 5 /// 7);("Sarah",1 /// 1);("Martin",6 /// 7);
    ("John",3 /// 7);("Kathin",6 /// 7)]


let (dilligente:fuzzy_set) = [("Marcus",5 /// 7);("Mary", 5 /// 10);("Sarah", 6 /// 10);
    ("Martin", 3 /// 7);("John", 5 /// 7);("Kathin",2 /// 7)]


let (talle:fuzzy_set) = [("Marcus",5 /// 10);("Mary",5 /// 10);("Sarah",5 /// 10);
    ("Martin",5 /// 10);("John",5 /// 10);("Kathin",5 /// 10)]


let (tall_tree:fuzzy_set) = [("Pine",Int 1);("Ash",frac 8);("Maple",1 /// 2);("Pear",Int 0);
    ("Larch",frac 4);("Oak",frac 5)]


let (timber_hardness:fuzzy_set) = [("Pine",frac 9);("Ash",frac 0);("Maple",frac 6);("Pear",frac 5);
    ("Larch",frac 0);("Oak",frac 8)]


let d1 = ["Pine"; "Ash"; "Maple"; "Pear"; "Larch"; "Oak"]

let d2 = ["Red";"Blue";"Green";"Orange";"Violet";"Turquoise"]
