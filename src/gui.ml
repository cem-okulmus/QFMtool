open GMain
open Base
open Sfquantifier
open Qfm
open List
open Rsvg

let _ = GtkMain.Main.init ()  (* needed to set up UI *)
let enter_query = GText.buffer ()
let answer = GText.buffer ()

let load_query parent ()=
  let dialog = GWindow.file_chooser_dialog 
      ~action:`OPEN ~decorated:true 
      ~title:"Open Query From File"
      ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  ignore $ dialog#set_current_folder (Sys.getcwd ());
  begin match dialog#run () with
  | `OPEN ->(
        match dialog#filename with
        Some a ->  enter_query#set_text (BatIO.read_all( BatFile.open_in a))
        | None -> () )
  | `DELETE_EVENT | `CANCEL -> ()
  end ;
BatIO.close_all ();
dialog#destroy ()

let save_query parent () = 
  let dialog = GWindow.file_chooser_dialog 
      ~action:`SAVE ~decorated:true 
      ~title:"Save Query To File"
      ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `SAVE `SAVE;
  ignore $ dialog#set_current_folder (Sys.getcwd ());
  begin match dialog#run () with
  | `SAVE ->(
        match dialog#filename with
        Some a ->  write_string (BatFile.open_out a) (enter_query#get_text ()); 
        | None -> () )
  | `DELETE_EVENT | `CANCEL -> ()
  end ;
  BatIO.close_all ();
  dialog#destroy ()

let evaluate_query s = 
  let lexbuf = Lexing.from_string s in 
  Qfm_parser.main Qfm_lexer.token lexbuf;;

let get_query_name s = 
  let lexbuf = Lexing.from_string s in 
  Qfm_parser.query_name Qfm_lexer.token lexbuf;;

(* Optional: Add information about arg list *)
let handle_query query = 
  let result  =  evaluate_query query in
  let query_name = get_query_name query  in
  query_name ++ " = " ++ (Num.string_of_num  result) ++ " ( ≈ " ++ 
  (BatFloat.round_to_string ~digits:2 $ Num.float_of_num result) ++ " )\n"

let query_callback (statbar: GMisc.statusbar_context) () = 
  let flash a = statbar#flash ~delay:5000 a in 
  try   
    Parser.flush_list_buffers ();
    answer#insert ~iter:(answer#get_iter `END) $ handle_query (enter_query#get_text ())
  with | Parsing.Parse_error         -> flash "Syntax error"
       | Parser.ObjectNotFound (a,b) -> flash ("No " ++ a ++ " of name \"" ++ b ++ "\" found")
       | Failure a                   -> flash ("Failure : " ++ a) 

let replace e () = 
  enter_query#delete ~start:(enter_query#get_iter `INSERT) ~stop:(enter_query#get_iter `SEL_BOUND); 
  enter_query#insert ~iter:(enter_query#get_iter `INSERT) e


(* Window for the plot viewer *)
let plot_window parent () = 
  let dialog = GWindow.dialog ~width:580 ~height:580
                              ~parent:parent ~title:"Plot selection" () in    
  let hbox = GPack.hbox ~homogeneous:true ~packing:dialog#vbox#add () in 
  let mode_sel = fst $ GEdit.combo_box_text ~active:0 ~height:1  ~packing:hbox#add 
                          ~strings:["Semi-fuzzy Quantifier"; "QFM"] () in
  let sfq_sel = GEdit.combo_box_text ~active:0 ~height:1 ~packing:hbox#add () 
                          ~strings:(map fst (rev !sfq_list))  in
  let qfm_sel = GEdit.combo_box_text ~show:false ~active:0 ~height:1 ~packing:hbox#add () 
                          ~strings: (map fst (rev !qfm_list)) in
  let induc_sel = GEdit.combo_box_text ~show:false ~active:0 ~height:1 ~packing:hbox#add () 
                          ~strings:["Negation";"Union";"Intersection"] in
  let image = GMisc.image ~packing:dialog#vbox#add () in
  
  let show_current_sel_sfq () = 
    begin match GEdit.text_combo_get_active sfq_sel with 
      | Some s -> image#set_pixbuf (render_from_file ?dpi:(Some 65.) 
                  ("Plots/SFQs/" ++ s  ++ ".svg" ));      
      | None -> ()
    end in 

  let show_current_sel_qfm () = 
    begin match GEdit.text_combo_get_active qfm_sel with 
      | Some s1 -> 
          begin match GEdit.text_combo_get_active induc_sel with 
            | Some s2 -> image#set_pixbuf (render_from_file ?dpi:(Some 65.) 
                         ("Plots/QFMs/" ++ s1  ++ s2 ++ ".svg" )); 
            | None -> () 
          end
      | None -> ()
    end in 

  ignore $ (fst sfq_sel)#connect#changed show_current_sel_sfq;
  ignore $ (fst qfm_sel)#connect#changed show_current_sel_qfm;
  ignore $ (fst induc_sel)#connect#changed show_current_sel_qfm;

  let state_one () = 
     show_current_sel_sfq ();
     (fst sfq_sel)#misc#show (); (fst qfm_sel)#misc#hide (); (fst induc_sel)#misc#hide () in
  let state_two () =
     show_current_sel_qfm ();
     (fst sfq_sel)#misc#hide (); (fst qfm_sel)#misc#show (); (fst induc_sel)#misc#show () in

  ignore $ mode_sel#connect#changed 
      ~callback: (fun () -> if mode_sel#active = 0 then state_one () else state_two ());

  show_current_sel_sfq ();
  dialog#show ();
   begin match dialog#run () with
    | `DELETE_EVENT | `CANCEL -> ()
   end;
   dialog#destroy ()

let start_ui () =
  let window = GWindow.window ~width:550 ~height:580
                              ~title:"QFM query tool" () in    
  let vbox = GPack.vbox ~packing:window#add () in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  let sfq_menu = factory#add_submenu "Select Quantifiers" in
  let qfm_menu = factory#add_submenu "Select QFMs" in
  
  let hbox = GPack.hbox ~homogeneous:true ~packing:vbox#add () in 
  (* Toolbar *)
  let toolbar = GButton.toolbar ~height:40 ~packing:vbox#pack () in
  let scroll = GBin.scrolled_window ~height:150  ~border_width:2 ~hpolicy:`AUTOMATIC 
              ~vpolicy:`AUTOMATIC ~packing:vbox#pack ~shadow_type: `ETCHED_OUT  () in  

  (* Query callback *)
  let query_button = toolbar#insert_button  ~text:"Answer query" ()  in 
  query_button#set_relief `NORMAL;
  toolbar#insert_space ();
  let plot_button = toolbar#insert_button  ~text:"Plotting tool" ()  in 
  plot_button#set_relief `NORMAL;

  let scroll_out = GBin.scrolled_window   ~border_width:2 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
                ~packing:hbox#pack ~shadow_type: `ETCHED_IN  () in  
  ignore $ GText.view ~buffer:enter_query  ~packing:scroll_out#add ();
  let answer_view = GText.view  ~editable:false ~buffer:answer  ~packing:scroll#add () in
  answer_view#misc#modify_base  [(`NORMAL, `RGB (60535,60535,60535))] ;

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore $ factory#add_item "Open query" ~key:GdkKeysyms._O ~callback: (load_query window);
  ignore $ factory#add_item "Save query" ~key:GdkKeysyms._S ~callback: (save_query window);  
  ignore $ factory#add_item "Quit"       ~key:GdkKeysyms._Q ~callback: Main.quit;

  (* SFQ and QFM menu *)
  let sf_factory =  new GMenu.factory sfq_menu ~accel_group in
  let qfm_factory =  new GMenu.factory qfm_menu ~accel_group in

  Sfquantifier.add_names ();
  Qfm.add_names ();
  let f = BatString.replace_chars (fun e -> if e = '_' then " " else Char.escaped e ) in

  ignore $ List.map (fun (e,_) -> sf_factory#add_item (f e) 
      ~callback: (replace (e ++ " 1") )) (List.rev !sfq_list_unary);

  ignore $ List.map (fun (e,_) -> sf_factory#add_item (f e) 
      ~callback: (replace e ))  (List.rev !sfq_list);

  ignore $ List.map (fun (e,_) -> sf_factory#add_item (f e) 
      ~callback: (replace (e ++ " 1 1 "))) (List.rev !sfq_list_giles);

  ignore $ List.map (fun (e,_) -> qfm_factory#add_item (f e) 
      ~callback: (replace  e )) (List. rev!qfm_list);
  
  (* Status bar *)
  let statbar = GMisc.statusbar ~height:20 ~has_resize_grip:false ~packing:vbox#pack () in 
  let statcontext = statbar#new_context ~name:"Gustav" in 
  ignore $ query_button#connect#clicked ~callback:(query_callback statcontext);
  ignore $ plot_button#connect#clicked ~callback:(plot_window window);
 
  (* Display the windows and enter Gtk+ main loop *)
  window#add_accel_group accel_group;
  ignore $ window#connect#destroy (Main.quit);
  window#show ();
  Main.main ()