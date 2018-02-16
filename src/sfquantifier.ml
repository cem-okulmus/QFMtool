open List
open Num
open Base
open Fuzzyset
open Cripset
open Threecut
open Replevels

type monotonicity = Non | Inc | Dec

type sf_quantor = 
    {
        q   : (crisp_set list) -> num;
        mon : monotonicity list; (* solely needed for the Cl qfms *)
    }
type ff_quantor = (fuzzy_set list) -> num

let sfq_list_unary : (string * (int -> sf_quantor)) list ref = ref []
let sfq_list_giles : (string * (int -> int -> sf_quantor) ) list ref = ref [] 
let sfq_list : (string * sf_quantor ) list ref = ref []

(* Zadeh's S-function, as described by Glöckner  *)
let s (alpha: num) (gamma:num) (x:num) =  match x,alpha,gamma with 
    | _ when x <=/ alpha                              
        -> Int 0 
    | _ when alpha </ x && x <=/ ((alpha +/ gamma) //  (Int 2) )
        -> (Int 2) */ (((x -/ alpha) // (gamma -/ alpha)) **/ (Int 2))  
    | _ when ((alpha +/ gamma) // (Int 2)) </ x && x <=/ gamma 
        -> (Int 1) -/ (Int 2) */ (((x -/ gamma) // (gamma -/ alpha)) **/ (Int 2))
    | _ -> Int 1

let unary_quantor mu mon :sf_quantor =
    let unary_quantor (mu: num -> num)  = function
        | [x] -> mu $ Int (length x)
        |  l  -> failure $ argument_error 1 (length l) in 
    { q = unary_quantor mu;  mon = mon; }

let relative_quantor_2 mu mon : sf_quantor =
    let relative_quantor_2 (mu: num -> num) = function
        | [[];b]    -> Int 1   (* if range is empty *)
        | [a ;b]    -> mu $ Int (length (inter a b)) // Int (length a)
        | l         -> failure $ argument_error 2 (length l) in
    { q = relative_quantor_2 mu;  mon = mon; }


let mu_around a b c x = match x with  (* Implicit assumption that a > b > c *)
    | _ when x <=/ a || x >=/ c     -> Int 0
    | _ when x >=/ b            -> (Int 1) -/ (x -/ b) // (c -/ b)
    | _                         -> (x -/ a) // (b -/ a)

let around_n n = 
    unary_quantor (mu_around (Int (n / 2)) (Int n) (Int (n + (n / 2)))) [Non]


let mu_almost_all x = s (frac 7) (frac 9) x

(* "Almost all X2s are X1s" == almost_all_2(X1,X2) *)
let almost_all_2 = relative_quantor_2 mu_almost_all [Dec;Inc]


let mu_around_half x = (s  (frac 25)  (frac 4) x) -/ (s  (frac 6)  (frac 75) x)

(* "Around half of X2s are X1s" == around_half_2(X1,X2) *)
let around_half_2 = relative_quantor_2 mu_around_half [Non;Non] (* "around_half" *)


let mu_few x = (Int 1) -/ s  (frac 1)  (frac 3) x

(* "Few of X2s are X1s" == few_2(X1,X2) *)
let few_2 = relative_quantor_2 mu_few  [Inc;Dec] (* "few" *)


let relative_simple_2 = relative_quantor_2 (fun a -> a)  [Dec;Inc] (* "equal " *)


let mu_exists a = if a > Int 0 then Int 1 else Int 0

let exists_2 = relative_quantor_2 mu_exists  [Dec;Inc](*  "Exists" *)


let mu_all a = if a = Int 1 then a else Int 0

let all_2 = relative_quantor_2 mu_all   [Dec;Inc] (* "All" *)


let mu_half a = max_num (Int 0) (min_num (Int 2 */ a ) (Int 2 -/ (Int 2 */ a)) )

let half_2 = relative_quantor_2 mu_half   [Non;Non](* "half" *)


let half_1 c = {q = (fun a -> half_2.q  [c;List.hd a]); mon = []}


let pro_e (e:element) (xs:crisp_set list) = match xs with 
    | [x]   -> cmem  x e
    | _     -> failure $ argument_error 1 (length xs)
let pro_e e = {q = pro_e e; mon = [Inc];}


let fuzzy_pro_e (e:element) (xs:fuzzy_set list) = match xs with 
    | [x]   -> fmem x e
    | _     -> failure $ argument_error 1 (length xs) 


(* Blind Choice quantifiers, from Fermüller and Roschger  *)

let l k m a = 
    let k = Int k in 
    let m = Int m in 
    min_num (Int 1) (max_num (Int 0) ((Int 1 ) +/ k -/ ((m +/ k ) */ a)))

let g k m a = 
    let k = Int k in 
    let m = Int m in 
    min_num (Int 1) (max_num (Int 0) ((Int 1) -/ k +/ ((m +/ k ) */ a)))

let h s t a = 
    min_num (g (s - t) (s + t) a ) (l  (s + t) (s - t) a) 


let l_2 k m = relative_quantor_2 (l k m) []

let g_2 k m = relative_quantor_2 (g k m) []

let h_2 s t = relative_quantor_2 (h s t) []

(* Deliberate Choice quantifier *)

let p k m a = 
    let k = Int k in 
    let m = Int m in 
    (binomial (k +/ m) k)  */ ((a) **/ k) */  (((Int 1) -/ (a)) **/ m)  

let w n quantifier =  
    { q= (fun arg ->  min_num (Int 1) ((Int n) */ quantifier.q arg)); 
    mon = quantifier.mon;}

let p_2 k m = relative_quantor_2 (p k m ) []


(*  Register quantifiers by string for the tool *)
let add_names () = 
    add sfq_list_unary ("around_n", around_n);;

    add sfq_list ("almost_all",almost_all_2);
    add sfq_list ("around_half",around_half_2);
    add sfq_list ("few",few_2);
    add sfq_list ("equal",relative_simple_2);
    add sfq_list ("exists",exists_2);
    add sfq_list ("all",all_2);
    add sfq_list ("half",half_2);

    (* Deal with the blind and deliberate differently *)
    add sfq_list_giles ("L",l_2);
    add sfq_list_giles ("G",g_2);
    add sfq_list_giles ("H",h_2);
    add sfq_list_giles ("P",p_2)