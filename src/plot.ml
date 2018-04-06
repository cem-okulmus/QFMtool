open List
open Base
open Num
open Sfquantifier
open Plplot

let plot_truth_2D ?sample:(sample=10000) (f: num -> num) filename =
    let xs = Array.init (sample+1) (fun a -> float_of_num (a /// sample)) in 
    let ys = Array.init (sample+1) (fun a -> float_of_num (f (a /// sample) )) in
    
    plscol0 0 255  255 255 ;
    plscol0 1 0  0 0 ;
   
	plsdev "svg";
    plsfnam (filename ++  ".svg");
    (* Initialize PLplot *)
    plinit ();
    (* Draw the plot window axes *)
    plenv (0.0) 1.0 0.0 1.0 0 0;
    plwidth 6.0;
    (* Draw the parabola points as a series of line segments *)
    plline xs ys;
    
    (* End the plotting session *)
    plend ()


let plot_truth_3D ?sample:(sample=100) (f: num -> num -> num) filename =
    let xs = Array.init sample (fun a -> (a /// (sample-1))) in 
    let ys = Array.init sample (fun a -> (a /// (sample-1))) in
    let zs = Array.make_matrix sample sample 0. in
    
    for i = 0 to sample-1  do 
        for j = 0 to sample-1  do 
        zs.(i).(j) <- float_of_num $ f xs.(i) ys.(j);
        done
    done;
    
    let xs = Array.map float_of_num xs in 
    let ys = Array.map float_of_num ys in 

    plscol0 0 255  255 255 ;
    plscol0 1 0  0 0 ;
    plscol0 2 0  0 0;
 
    plsdev "svg";
    plsfnam (filename ++  ".svg");
    plinit ();

    let i = [|0.0; 1.0|] in (* Left and right bounds *)
    let h = [|240.0; 0.0|] in(* blue -> green -> yellow -> red *)
    let l = [|0.6; 0.6|] in
    let s = [|0.8; 0.8|] in
    plscmap1n 256;
    plscmap1l false i h l s None;
    
    pladv 0;
    plcol0 1;
    plvpor (-0.1) 1.1 (-0.22) 1.0;
    plwind (-1.0) 1.0 (-1.0) 1.5;
    plw3d 1.0 1.0 1.2 0.0 1.0 (0.0) 1.0 0.0 1.0 33.0 60.0;
    plbox3 "bnstu" "" 0.0 0
           "bnstu" "" 0.0 0
           "bcdmnstuv" "" 0.0 0;

    plcol0 2;

    plmesh xs ys zs [PL_DRAW_LINEXY;];


    plcol0 3;
    plmtex "t" 1.0 0.5 0.5 "";

    (* End the plotting session *)
    plend ()

(* Generate the plots for the UI *)
let generate_svgs () = 
    (* SFQs *)
    let directory = "Plots/SFQs/" in 
    plot_truth_2D mu_almost_all (directory ++ "almost_all");
    plot_truth_2D mu_around_half (directory ++ "around_half");
    plot_truth_2D mu_few (directory ++ "few");
    plot_truth_2D (fun a -> a) (directory ++ "equal");
    plot_truth_2D mu_exists (directory ++ "exists");
    plot_truth_2D mu_all (directory ++ "all");
    plot_truth_2D mu_half (directory ++ "half");
    (* QFMs *)
    let directory = "Plots/QFMs/" in
    Qfm.add_names ();
    ignore $ map (fun (a,b) -> plot_truth_2D (Dfsaxioms.induced_neg b) 
                               (directory ++ a ++ "Negation")) !Qfm.qfm_list;
    ignore $ map (fun (a,b) -> plot_truth_3D (Dfsaxioms.induced_or b ) 
                               (directory ++ a ++ "Union")) !Qfm.qfm_list;
    ignore $ map (fun (a,b) -> plot_truth_3D (Dfsaxioms.induced_and b) 
                               (directory ++ a ++ "Intersection")) !Qfm.qfm_list;