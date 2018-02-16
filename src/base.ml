open List
open Num

(* basic helper functions *)

let ( $ ) f a = f (a)

let ( $$ ) f g a = f (g a)

let failure a = raise $ Failure a

let unit = Random.init $ int_of_float ( Unix.time ())

let ( ++ ) a b = String.concat "" [a;b]

let argument_error a b =
    string_of_int b ++ " argument(s) given, but expected " ++ string_of_int a

let reduce = BatList.reduce

let (@) = BatList.append

let rec cartesian temp = function
  | [] -> temp
  | h :: t ->
    let newthings = rev_map (fun a -> [a]) h in
    let temp = BatList.concat $ rev_map (fun i -> rev_map (fun r -> i @ r) newthings ) temp in
    cartesian temp t ;;

let cartesian list = cartesian [[]] list

let init = BatList.init

let of_float = BatNum.of_float

let rec binomial n k = match n,k with
    | _ when n < k -> Int 0
    | _ when n = k -> Int 1
    | _ -> ((binomial (n -/ Int 1 ) k) */  n) // (n -/ k)

let rev_compare a b = compare_num a b * -1

(* produce for n -> 0.n  *)
let frac n =
    let rec pot ?a:(a=Int 10) b = if (a >/ b) then a else pot ~a:(a */ (Int 10)) b in
    (Int n) // pot (Int n)

let print_num fmt t = Format.fprintf fmt "%s" (BatFloat.round_to_string ~digits:8 (float_of_num t))

let (///) a b = (Int a // Int b)

let add l a = l := a::!l

(* Łukasiewicz t-norm *)
let (&) a b =
    max_num (Int 0) (a +/ b -/ (Int 1))

let range_init from too f =  map f (BatList.range from `To too)

let write_string output string =
    init (String.length  string) (String.get string)
    |> iter (BatIO.write output) 
