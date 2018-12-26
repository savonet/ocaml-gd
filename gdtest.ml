(* $Header: /home/cvs/gd4o/gdtest.ml,v 1.7 2003/11/25 01:02:32 matt Exp $ *)
(*
 * GD4O: An OCaml interface to the Gd graphics library.
 * Based on Shawn Wagner's OCamlGD 0.7.0.
 * Copyright (C) 2002  Shawn Wagner
 * Copyright (C) 2003  Matthew C. Gushee
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Gd;;


let write_header ch title desc =
  output_string ch ("************************************" ^
                    "************************************\n\n");
  output_string ch (title ^ ":\n");
  output_string ch (desc ^ "\n\n");
  output_string ch ("------------------------------------" ^
                    "------------------------------------\n\n\n")

let write_footer ch msg =
  output_string ch ("\n\n" ^ msg ^ "\n");
  output_string ch ("====================================" ^
                    "====================================\n\n\n")

let ok n msg = "   " ^ string_of_int n ^ ": [--OK--]: " ^ msg ^ "\n"
let failed n msg = "   " ^ string_of_int n ^ ": [FAILED]: " ^ msg ^ "\n"

let skiplineRE = Str.regexp "^\\([ \t]*#.*\\|[ \t]*\\)$"

let deg2rad = ( *. ) (3.1415926535 /. 180.)

let tempdir =
  try
    Sys.getenv "TMP"
  with Not_found ->
    begin
      try
        Sys.getenv "TEMP"
      with Not_found ->
        if Sys.file_exists "/tmp" then "/tmp"
        else if Sys.file_exists "/var/tmp" then "/var/tmp"
        else Sys.getcwd ()
    end;;


let shapes_test msg_ch =
  write_header msg_ch "SHAPES"
    ( "Create an image displaying various interesting shapes.\n" ^
      "Save output as 'shapes_test.png'.");
  let msg = output_string msg_ch in
  let img =
    try
      let i = create 512 384 in
      msg (ok 1 "created image");
      i
    with _ -> failwith "Failed to create image." in
  let ca =
    try
      let c = img#colors in
      msg (ok 2 "obtained color allocator instance");
      c
    with _ -> failwith "Failed to get color allocator." in
  let white =
    try
      let w = ca#white in
      msg (ok 3 "allocated white");
      w
    with _ -> failwith "Failed to get white."
  and ltblue, maroon, orange, aqua, grey, brown =
    try
      let more =
        ca#create 127 127 255, ca#create 127 0 0, ca#create 191 127 0,
        ca#create 0 153 204, ca#create 153 153 153, ca#create 127 127 0 in
      msg (ok 4 "allocated additional colors");
      more
    with _ -> failwith "Failed to create additional colors." in
  begin
    try
      img#rectangle 10 10 180 140 maroon;
      msg (ok 5 "drew outline rectangle")
    with _ -> msg (failed 5 "Failed to draw outline rectangle.")
  end;
  begin
    try
      img#filled_rectangle 50 40 220 170 ltblue;
      msg (ok 6 "drew filled rectangle")
    with _ -> msg (failed 6 "Failed to draw filled rectangle.")
  end;
  begin
    try
      img#set_antialiased aqua;
      begin
        try
          img#arc ~cx:108 ~cy:276 ~w:192 ~h:128 ~s:0 ~e:270 
            ~pseudo:(ca#antialiased ()) aqua;
          msg (ok 7 "drew partial outline arc with antialiased pseudocolor")
        with _ -> msg (failed 7 "Failed to draw partial outline arc.")
      end
    with _ -> msg (failed 7 "Failed to set antialiased color.")
  end;
  begin
    try
      img#filled_ellipse 144 300 192 128 grey;
      msg (ok 8 "drew filled ellipse")
    with _ -> msg (failed 8 "Failed to draw filled ellipse.")
  end;
  begin
    try
      img#set_antialiased brown;
      begin
        try
          img#polygon
            ~pts:[|280,80;340,32;440,48;440,144;360,144;280,108;280,80|] 
            ~pseudo:(ca#antialiased ()) brown;
          msg (ok 9 "drew outline polygon with antialiased pseudocolor")
        with _ -> msg (failed 9 "Failed to draw polygon.")
      end
    with _ -> msg (failed 9 "Failed to set antialiased color.")
  end;
  begin
    try
      img#filled_polygon 
        [|320,110;380,62;480,78;480,174;400,174;320,138;320,110|] orange;
      msg (ok 10 "drew filled polyon")
    with _ -> msg (failed 10 "Failed to draw filled polygon.")
  end;
  begin
    try
      img#string
        ~x:244 ~y:228 ~font:Gd.Font.giant 
        ~s:"As I was coming up the stair," orange;
      msg (ok 11 "drew string 1")
    with _ -> msg (failed 11 "Failed to draw string 1.")
  end;
  begin
    try
      img#string
        ~x:254 ~y:260 ~font:Gd.Font.large 
        ~s:"I met a man who wasn't there." grey;
      msg (ok 12 "drew string 2")
    with _ -> msg (failed 12 "Failed to draw string 2.")
  end;
  begin
    try
      img#string
        ~x:264 ~y:290 ~font:Gd.Font.medium
        ~s:"He wasn't there again today!" aqua;
      msg (ok 13 "drew string 3")
    with _ -> msg (failed 13 "Failed to draw string 3.")
  end;
  begin
    try
      img#string
        ~x:274 ~y:318 ~font:Gd.Font.small 
        ~s:"I wish, I wish, he'd go away." maroon;
      msg (ok 14 "drew string 4")
    with _ -> msg (failed 14 "Failed to draw string 4.")
  end;
  img#save_as_png (Filename.concat tempdir "shapes_test.png");
  write_footer msg_ch "END SHAPES"


let color_allocation_test msg_ch =
  write_header msg_ch "COLOR ALLOCATION"
    ("Create an 8-bit image and a truecolor image, and attempt to allocate\n"^
     "a large number of colors in each. The test should fail at index 256 \n"^
     "for the 8-bit image, and should *not* fail for the truecolor image.");
  let msg = output_string msg_ch in
  let rgbvals = [|0; 31; 63; 95; 127; 159; 191; 223; 255|] in
  let numvals = Array.length rgbvals in
  let last = numvals - 1 in
  let do_colors img is_tc testno =
    let (ca : color_allocator) = img#colors
    and index = ref 0 in
    try
      for r = 0 to last do
        for g = 0 to last do
          for b = 0 to last do
            index := (r * numvals * numvals + g * numvals + b);
            ignore (ca#create rgbvals.(r) rgbvals.(g) rgbvals.(b))
          done;
        done;
      done;
      if is_tc then
        msg (ok testno "Truecolor - all colors successfully allocated.")
      else
        msg (failed testno "8bit - too many colors allocated without error.")
    with
    | Too_many_colors ->
      if is_tc then
        msg (failed testno ("Truecolor - failed at index " ^ 
                             string_of_int !index ^ ".")) else
      if !index = 256 then
        msg (ok testno "8bit - failed at index 256.") 
      else
        msg (failed testno ("8bit - failed at index " ^ 
                             string_of_int !index ^ "."))
    | _ ->
      if is_tc then msg (failed testno ("Truecolor - unknown exception " ^ 
                                        "at index " ^
                                        string_of_int !index ^ "."))
      else msg (failed testno ("8bit - unknown exception at index " ^
                                string_of_int !index ^ ".")) in
  do_colors (create 256 256) false 1;
  do_colors (create_truecolor 256 256) true 2;
  write_footer msg_ch "END COLOR ALLOCATION"

      
let copy_resize_test msg_ch =
  write_header msg_ch "COPYING AND RESIZING"
    ( "Test copying and resizing functions.\n" ^
      "1. Copy a small image into a larger image with 'copy'.\n" ^
      "2. Copy a portion of an image into a larger image with 'copy'.\n" ^
      "3. Copy a small image into a larger image with 'copy_resized'.\n" ^
      "4. Copy a small image into a larger image with 'copy_resampled'.\n" ^
      "   Compare the output with that of test #2; details should be\n" ^
      "   smoother.\n" ^
      "5. Copy a small image into a larger image with 'copy_rotated'.\n" ^
      "6. Copy a small image into a larger image with 'copy_merge'.\n" ^
      "7. Copy a small image into a larger image with 'copy_merge_gray'.\n" ^
      "8. Create a new image, copy the palette from an existing image,\n" ^
      "   then copy the contents of the second image. If the palette is\n" ^
      "   copied correctly, this should produce a copy whose colors and\n" ^
      "   dimensions are identical to the original.\n");
  let msg = output_string msg_ch in
  let indexed_img1 = "yotei02-8.png"
  and truecolor_img1 = "yotei02-t.png"
  and indexed_img2 = "driver01-8.png"
  and truecolor_img2 = "driver01-t.png"
  and indexed_img3 = "kamokamogawa04-8.png"
  and truecolor_img3 = "kamokamogawa04-t.png" in
  let img_path name = 
    Filename.concat "." (Filename.concat "samples" name) in

  let get_dims dst src = dst#width, dst#height, src#width, src#height in
  
  let test1 img1 img2 =
    let dst = open_png (img_path img1)
    and src = open_png (img_path img2) in
    try
      let outfile = "copy.png" in
      let dw, dh, sw, sh = get_dims dst src in    
      dst#copy src ~x:((dw - sw) / 2) ~y:((dh - sh) / 2)
        ~src_x:0 ~src_y:0 ~w:sw ~h:sh;
      dst#save_as_png (Filename.concat tempdir outfile);
      msg (ok 1 ("Saved " ^ outfile ^ "."))
    with _ ->
      msg (failed 1 "image#copy test failed.")

  and test2 img1 img2 =
    let dst = open_png (img_path img1)
    and src = open_png (img_path img2) in
    try
      let outfile = "copy(crop).png" in
      let dw, dh, sw, sh = get_dims dst src in    
      let crop_x = int_of_float ((float_of_int sw) *. 0.35)
      and crop_y = int_of_float ((float_of_int sh) *. 0.35) in
      dst#copy src ~x:(dw / 2) ~y:(dh / 2)
        ~src_x:crop_x ~src_y:crop_y ~w:(sw / 2) ~h:(sh / 2);
      dst#save_as_png (Filename.concat tempdir outfile);
      msg (ok 2 ("Saved " ^ outfile ^ "."))
    with _ ->
      msg (failed 2 "image#copy cropping test failed.")

  and test3 img1 img2 =
    let dst = open_png (img_path img1)
    and src = open_png (img_path img2) in
    try
      let outfile = "copy_resized.png" in 
      let dw, dh, sw, sh = get_dims dst src in    
      let new_w = (int_of_float ((float_of_int sw) *. 0.75))
      and new_h = (int_of_float ((float_of_int sh) *. 1.1)) in
      dst#copy_resized src ~x:((dw - new_w) / 2) ~y:((dh - new_h) / 2)
        ~src_x:0 ~src_y:0 ~src_w:sw ~src_h:sh ~w:new_w ~h:new_h;
      dst#save_as_png (Filename.concat tempdir outfile);
      msg (ok 3 ("Saved " ^ outfile ^ "."))
    with _ ->
      msg (failed 3 "image#copy_resized test failed.")

  and test4 img1 img2 =
    let dst = open_png (img_path img1)
    and src = open_png (img_path img2) in
    try
      let outfile = "copy_resampled.png" in
      let dw, dh, sw, sh = get_dims dst src in    
      let new_w = (int_of_float ((float_of_int sw) *. 0.75))
      and new_h = (int_of_float ((float_of_int sh) *. 1.1)) in
      dst#copy_resampled src ~x:((dw - new_w) / 2) ~y:((dh - new_h) / 2)
        ~src_x:0 ~src_y:0 ~src_w:sw ~src_h:sh ~w:new_w ~h:new_h;
      dst#save_as_png (Filename.concat tempdir outfile);
      msg (ok 4 ("Saved " ^ outfile ^ "."))
    with _ ->
      msg (failed 4 "image#copy_resampled test failed.")

  and test5 img1 img2 =
    let dst = open_png (img_path img1)
    and src = open_png (img_path img2) in
    try
      let outfile = "copy_rotated.png" in
      let dw, dh, sw, sh = get_dims dst src in    
      let cx = (float_of_int dw) /. 2.
      and cy = (float_of_int dh) /. 2. in
      dst#copy src ~x:((dw - sw) / 2) ~y:((dh - sh) / 2)
        ~src_x:0 ~src_y:0 ~w:sw ~h:sh;
      dst#copy_rotated src ~x:cx ~y:cy
        ~src_x:0 ~src_y:0 ~w:sw ~h:sh ~angle:30;
      dst#copy_rotated src ~x:cx ~y:cy
        ~src_x:0 ~src_y:0 ~w:sw ~h:sh ~angle:60;
      dst#copy_rotated src ~x:cx ~y:cy
        ~src_x:0 ~src_y:0 ~w:sw ~h:sh ~angle:90;
      dst#save_as_png (Filename.concat tempdir outfile);
      msg (ok 5 ("Saved " ^ outfile ^ "."))
    with _ ->
      msg (failed 5 "image#copy_rotated test failed.")

  and test6 img =
    let src = open_png (img_path img) in
    let sw = src#width
    and sh = src#height in
    let dw = sw * 2
    and dh = sh * 2 in
    let dst = create ~x:dw ~y:dh in
    try
      let outfile = "copy_merge.png" in
      let dw, dh, sw, sh = get_dims dst src in
      let ca = dst#colors in
      ignore (ca#create 0 204 153);
      dst#copy_merge src ~x:((dw - sw) / 2) ~y:((dh - sh) / 2)
        ~src_x:0 ~src_y:0 ~w:sw ~h:sh ~pct:50;
      dst#save_as_png (Filename.concat tempdir outfile);
      msg (ok 6 ("Saved " ^ outfile ^ "."))
    with _ ->
      msg (failed 6 "image#copy_merge test failed.")

  and test7 img =
    let src = open_png (img_path img) in
    let sw = src#width
    and sh = src#height in
    let dw = sw * 2
    and dh = sh * 2 in
    let dst = create ~x:dw ~y:dh in
    try
      let outfile = "copy_merge_gray.png" in
      let dw, dh, sw, sh = get_dims dst src in
      let ca = dst#colors in
      ignore (ca#create 0 204 153);
      dst#copy_merge_gray src ~x:((dw - sw) / 2) ~y:((dh - sh) / 2)
        ~src_x:0 ~src_y:0 ~w:sw ~h:sh ~pct:50;
      dst#save_as_png (Filename.concat tempdir outfile);
      msg (ok 7 ("Saved " ^ outfile ^ "."))
    with _ ->
      msg (failed 7 "image#copy_merge_gray test failed.")

  and test8 img =
    let src = open_png (img_path img) in
    let sw = src#width
    and sh = src#height in
    let dw = sw
    and dh = sh in
    let dst = create ~x:dw ~y:dh in
    try
      let outfile = "palette_copy.png" in
      let dw, dh, sw, sh = get_dims dst src in    
      dst#palette_copy src;
      dst#copy src ~x:0 ~y:0 ~src_x:0 ~src_y:0 ~w:sw ~h:sh;
      dst#save_as_png (Filename.concat tempdir outfile);
      msg (ok 8 ("Saved " ^ outfile ^ "."))
    with _ ->
      msg (failed 8 "image#palette_copy test failed.") in

  test1 indexed_img1 indexed_img2;
  test2 indexed_img1 indexed_img2;
  test3 truecolor_img1 indexed_img2;
  test4 truecolor_img1 indexed_img2;
  test5 truecolor_img1 truecolor_img2;
  test6 indexed_img2;
  test7 indexed_img2;
  test8 indexed_img3;

  write_footer msg_ch "END COPYING AND RESIZING"


let io_test msg_ch =
  write_header msg_ch "INPUT/OUTPUT"
    ( "Test loading and saving functions.\n" ^
      "1. Save to PNG with 'save_to_png'.\n" ^
      "2. Save to JPEG with 'save_to_jpeg,' quality 100.\n" ^
      "3. Save to JPEG with 'save_to_jpeg,' quality 60.\n" ^
      "4. Save to PNG with 'open_out' -> 'out_as_png'.\n" ^
      "5. Save to JPEG with 'open_out' -> 'out_as_jpeg'.\n"); 
  let msg = output_string msg_ch in
  let draw () =
    let img = create 256 256 in
    let ca = img#colors in
    let white = ca#white
    and red = ca#create 255 0 0
    and black = ca#black in
    img#filled_ellipse 64 64 100 100 red;
    img#filled_ellipse 192 64 100 100 black;
    img#filled_ellipse 64 192 100 100 black;
    img#filled_ellipse 192 192 100 100 red;
    img in
  let fname = (Filename.concat tempdir "io_test-d.png")
  and im = draw () in
  begin
    try
      im#save_as_png fname;
      msg (ok 1 "Saved " ^ fname)
    with _ -> msg (failed 1 "Failed to save " ^ fname)
  end;
  
  let fname = Filename.concat tempdir "io_test-d-100.jpg"
  and im = draw () in
  begin
    try
      im#save_as_jpeg ~quality:100 fname;
      msg (ok 2 "Saved " ^ fname);
    with _ -> msg (failed 2 "Failed to save " ^ fname)
  end;
  
  let fname = Filename.concat tempdir "io_test-d-60.jpg"
  and im = draw () in
  begin
    try
      im#save_as_jpeg ~quality:60 fname;
      msg (ok 3 "Saved " ^ fname);
    with _ -> msg (failed 3 "Failed to save " ^ fname)
  end;
  
  let fname = Filename.concat tempdir "io_test-i.png"
  and im = draw ()
  and oc = open_out fname in
  begin
    try
      im#out_as_png oc;
      msg (ok 4 "Saved " ^ fname);
    with _ -> msg (failed 4 "Failed to save " ^ fname)
  end;
  close_out oc;
  
  let fname = Filename.concat tempdir "io_test-i.jpg"
  and im = draw ()
  and oc = open_out fname in
  begin
    try
      im#out_as_jpeg oc;
      msg (ok 5 "Saved " ^ fname);
    with _ -> msg (failed 5 "Failed to save " ^ fname)
  end;
  close_out oc;
  write_footer msg_ch "END INPUT/OUTPUT"


let ft_test msg_ch fonts =
  try
    let [ f1; f2; f3; f4 ] = fonts in
    write_header msg_ch "TRUETYPE FONT RENDERING"
      ( "Test of #image#string_ft. Create an image with a white\n" ^
        "background and four strings with the following properties:\n" ^
        "  1. 24pt, blue, start at x:60,y:236, slopes upward 30 deg.\n" ^
        "  2. 18pt, grey, start at x:60,y:272\n" ^
        "  3. 12pt, red, start at x:60,y:96\n" ^
        "  4. 8pt, black, start at x:60,y:316\n" ^
        "Save output as 'ft_test.png'.");
    let msg = output_string msg_ch in
    let im = create 512 384 in
    let ca = im#colors in
    let white = ca#white
    and black = ca#black
    and grey = ca#create 153 153 153
    and red = ca#create 204 0 0
    and blue = ca#create 47 47 255 in
    ignore (im#string_ft blue f1 24.0 (deg2rad 30.) 60 236 
      "As I was coming up the stair,");
    msg (ok 1 "Rendered first string.");
    ignore (im#string_ft grey f2 18.0 0.0 60 272 
      "I met a man who wasn't there.");
    msg (ok 2 "Rendered second string.");
    ignore (im#string_ft red f3 12.0 0.0 60 296 
      "He wasn't there again today!");
    msg (ok 3 "Rendered third string.");
    ignore (im#string_ft black f4 8.0 0.0 60 316 
      "I wish, I wish, he'd go away.");
    msg (ok 4 "Rendered fourth string.");
    im#save_as_png (Filename.concat tempdir "ft_test.png");
    write_footer msg_ch "END TRUETYPE FONT RENDERING"
  with Match_failure _ ->
    prerr_endline 
      "The TrueType font test requires a list of four font files."
      

let ftex_test msg_ch fonts =
  try
    let fnt = List.hd fonts in
    write_header msg_ch "TRUETYPE FONT EXTENDED RENDERING"
      ( "Test of #image#string_ftex. Create an image with a white\n" ^
        "background and a four-line poem in black 14pt type, widely\n" ^
        "spaced (2.5 * line height); save output as 'ftex_test.png'.");
    let msg = output_string msg_ch in
    let im = create 512 384 in
    let ca = im#colors in
    let white = ca#white
    and black = ca#black in
    let poem = ( "As I was coming up the stair,\n" ^ 
      "I met a man who wasn't there.\n" ^
      "He wasn't there again today!\n" ^
      "I wish, I wish he'd go away." ) in
    ignore (im#string_ftex ~fg:black ~fname:fnt ~size:14.0 ~angle:0.0
      ~x:100 ~y:80 ~flags:[|FTExSetSpacing|] ~spacing:2.5 poem);
    msg (ok 1 "Rendered poem.");
    im#save_as_png (Filename.concat tempdir "ftex_test.png");
    msg (ok 2 "Saved to 'ftex_test.png'");
    write_footer msg_ch "END TRUETYPE FONT EXTENDED RENDERING"
  with Match_failure _ ->
    prerr_endline 
      "The TrueType font test requires a list of four font files."

let ft_tests msg_ch =
  let rec getfonts ch fnts =
    if List.length fnts >= 4 then fnts
    else
      ( try
          let line = input_line ch in
          if Str.string_match skiplineRE line 0 then
            getfonts ch fnts
          else
            getfonts ch (line :: fnts)
        with End_of_file -> fnts ) in
  if Sys.file_exists "font.list" then
    let ic = open_in "font.list" in
    let fonts = List.rev (getfonts ic []) in
    close_in ic;
    ft_test msg_ch fonts;
    ftex_test msg_ch fonts
  else
    prerr_endline
      ( "IMPORTANT: if you wish to run the TrueType font rendering tests,\n" ^
        "you must have a file named 'font.list' which contains the full\n" ^
        "pathnames of four font files that exist on your system. For an\n" ^
        "example, see 'font_list.txt'" )
      

let test msg_ch =
  color_allocation_test msg_ch;
  shapes_test msg_ch;
  copy_resize_test msg_ch;
  io_test msg_ch;
  ft_tests msg_ch


let _ =
  prerr_endline ("::::::::::::::::::::::::::::::::::::" ^
                 "::::::::::::::::::::::::::::::::::::");
  prerr_endline ("Starting tests. Files will be saved in " ^ tempdir ^ ".");
  prerr_endline ("::::::::::::::::::::::::::::::::::::" ^
                 "::::::::::::::::::::::::::::::::::::");
  if Array.length Sys.argv >= 2 then
    let logfile = Sys.argv.(1) in
    let msgchannel = open_out logfile in
    test msgchannel;
    prerr_endline ("Tests completed. For details, see " ^ logfile ^ ".");
    close_out msgchannel
  else
    begin
      test stderr;
      prerr_endline "Tests completed."
    end
