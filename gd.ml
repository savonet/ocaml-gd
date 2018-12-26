(* $Header: /home/cvs/gd4o/gd.ml,v 1.6 2003/11/25 01:02:32 matt Exp $ *)
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


exception Too_many_colors
exception Color_not_found
exception Image_creation_failed 
exception Not_supported
exception Illegal_state of string
exception GD_Freetype_exception of string

let _ = Callback.register_exception "gdopen failed" Image_creation_failed
let _ = Callback.register_exception "gd type not supported" Not_supported
let _ = Callback.register_exception "gd freetype exception" (GD_Freetype_exception "msg")

type t (* Image type *)
type c = int (* Color type *)
type font (* Font type *)

type ftex_flag =
  | FTExSetSpacing
  | FTExSetCharmap
type ftex_charmap =
  | FTExUnicode
  | FTExShiftJIS
  | FTExBig5

(*
class virtual color =
object
  method virtual red_part: int
  method virtual green_part: int
  method virtual blue_part: int
  method virtual code: int
  method virtual antialiased: color
  method virtual is_aa: bool
end
*)
type color = {
  red_channel : int;
  green_channel : int;
  blue_channel : int;
  alpha_channel : int;
  index : int;
}

class virtual color_allocator =
  object
    method virtual create: red:int -> green:int -> blue:int -> color
    method virtual closest: red:int -> green:int -> blue:int -> color
    method virtual closest_hwb: red:int -> green:int -> blue:int -> color
    method virtual resolve: red:int -> green:int -> blue:int -> color
    method virtual exact: red:int -> green:int -> blue:int -> color
    method virtual find: red:int -> green:int -> blue:int -> color
    method virtual get_color_by_index: int -> color
    method virtual white: color
    method virtual black: color
    method virtual blue: color
    method virtual green: color
    method virtual red: color
    method virtual get_transparent: color
    method virtual set_transparent: color -> unit
    method virtual set_antialiased: bool -> unit
    method virtual set_brushed: bool -> unit
    method virtual set_styled: bool -> unit
    method virtual set_tiled: bool -> unit
    method virtual antialiased: unit -> int
    method virtual brushed: unit -> int
    method virtual styled: unit -> int
    method virtual styled_brushed: unit -> int
    method virtual tiled: unit -> int
    method virtual transparent: unit -> int
  end

class virtual image =
  object
    method virtual get_image: t
    method virtual colors: color_allocator
    method virtual line: x1:int -> y1:int -> x2:int -> y2:int -> 
        ?pseudo:int -> color -> unit
    method virtual dashed_line: x1:int -> y1:int -> x2:int -> y2:int -> 
        ?pseudo:int -> color -> unit
    method virtual rectangle: x1:int -> y1:int -> x2:int -> y2:int -> 
        ?pseudo:int -> color -> unit
    method virtual filled_rectangle: x1:int -> y1:int -> x2:int -> y2:int -> 
        ?pseudo:int -> color -> unit
    method virtual polygon: pts:(int * int) array -> ?pseudo:int -> 
        color -> unit
    method virtual filled_polygon: pts:(int * int) array -> ?pseudo:int -> 
        color -> unit
    method virtual arc: cx:int -> cy:int -> w:int -> h:int -> s:int -> 
        e:int -> ?pseudo:int -> color -> unit
    method virtual closed_arc:
        cx:int -> cy:int -> w:int -> h:int -> s:int -> e:int ->
        ?nofill:bool -> ?edged:bool -> ?pseudo:int -> color -> unit
    method virtual closed_chord:
        cx:int -> cy:int -> w:int -> h:int -> s:int -> e:int ->
        ?nofill:bool -> ?edged:bool -> ?pseudo:int -> color -> unit 
    method virtual filled_ellipse: cx:int -> cy:int -> w:int -> h:int -> 
        ?pseudo:int -> color -> unit
    method virtual border_fill: x:int -> y:int -> border:color -> 
        fill:color -> unit
    method virtual fill: x:int -> y:int -> color -> unit
    method virtual set_antialiased: color -> unit
    method virtual set_antialiased_dont_blend: 
      aacolor:color -> dontblend:color -> unit
    method virtual set_brush: image -> unit
    method virtual set_tile: image -> unit
    method virtual set_thickness: int -> unit
    method virtual set_clip: x1:int -> y1:int -> x2:int -> y2:int -> unit
    method virtual save_as_png: string -> unit
    method virtual save_as_jpeg: ?quality:int -> string -> unit
    method virtual out_as_png: out_channel -> unit
    method virtual out_as_jpeg: ?quality:int -> out_channel -> unit
    method virtual set_pixel: x:int -> y:int -> color -> unit
    method virtual get_pixel: x:int -> y:int -> color
    method virtual width: int
    method virtual height: int
    method virtual in_range: x:int -> y:int -> bool
    method virtual letter: font:font -> x:int -> y:int -> c:char ->
      color -> unit
    method virtual letter_up: font:font -> x:int -> y:int -> c:char -> 
      color -> unit
    method virtual string: font:font -> x:int -> y:int -> s:string -> 
      color -> unit
    method virtual string_up: font:font -> x:int -> y:int -> s:string -> 
      color -> unit
    method virtual string_ft:
      fg:color -> fname:string -> size:float -> angle:float -> 
      x:int -> y:int -> string -> int array
    method virtual string_ftex:
      fg:color -> fname:string -> size:float -> angle:float -> 
      x:int -> y:int -> ?flags:ftex_flag array -> ?spacing:float -> 
      ?charmap:ftex_charmap -> string -> int array
    method virtual copy: image -> x:int -> y:int -> src_x:int -> src_y:int ->
      w:int -> h:int -> unit
    method virtual copy_resized: image -> x:int -> y:int -> src_x:int -> 
      src_y:int -> w:int -> h:int -> src_w:int -> src_h:int -> unit
    method virtual copy_resampled: image -> x:int -> y:int -> src_x:int -> 
      src_y:int -> w:int -> h:int -> src_w:int -> src_h:int -> unit
    method virtual copy_rotated: image -> x:float -> y:float -> src_x:int ->
      src_y:int -> w:int -> h:int -> angle:int -> unit
    method virtual copy_merge: image -> x:int -> y:int -> src_x:int ->
      src_y:int -> w:int -> h:int -> pct:int -> unit
    method virtual copy_merge_gray: image -> x:int -> y:int -> src_x:int ->
      src_y:int -> w:int -> h:int -> pct:int -> unit
    method virtual palette_copy: image -> unit 
  end

(* Private interface routines. *)
(* Create an image *)
external do_image_create: int -> int -> t = "ml_image_create"
external do_image_create_truecolor: int -> int -> t
    = "ml_image_create_truecolor"
external do_image_open_png: string -> t = "ml_image_open_png"
external do_image_open_jpeg: string -> t = "ml_image_open_jpeg"

external do_is_truecolor: t -> bool = "ml_image_is_truecolor"

(* Drawing functions *)        
external do_set_pixel: t -> int -> int -> int -> unit = "ml_set_pixel"

external do_get_pixel: t -> int -> int -> int = "ml_get_pixel"

external do_get_width: t -> int = "ml_get_width"
external do_get_height: t -> int = "ml_get_height"

external do_draw_line: t -> int -> int -> int -> int -> int -> int -> unit
    = "ml_image_line" "ml_image_line_native"

external do_draw_dline: t -> int -> int -> int -> int -> int -> int -> unit
    = "ml_image_dline" "ml_image_dline_native"

external do_draw_rect: t -> int -> int -> int -> int -> int -> int -> unit
    = "ml_image_rect" "ml_image_rect_native"

external do_draw_frect: t -> int -> int -> int -> int -> int -> int -> unit
    = "ml_image_frect" "ml_image_frect_native"

external do_draw_poly: t -> (int * int) array -> int -> int -> int -> unit
    = "ml_image_poly"

external do_draw_fpoly: t -> (int * int) array -> int -> int -> int -> unit
    = "ml_image_fpoly"

external do_draw_arc:
  t -> int -> int -> int -> int -> int -> int -> int -> int -> unit
    = "ml_image_arc" "ml_image_arc_native"

external do_draw_carc:
  t -> int -> int -> int -> int -> int -> int -> int -> int -> bool -> bool -> unit
  = "ml_image_carc" "ml_image_carc_native"

external do_draw_cchord:
  t -> int -> int -> int -> int -> int -> int -> int -> int -> bool -> bool -> unit
  = "ml_image_cchord" "ml_image_cchord_native"

external do_draw_fell:
  t -> int -> int -> int -> int -> int -> int -> unit
    = "ml_image_fell" "ml_image_fell_native"

external do_border_fill: t -> int -> int -> int -> int -> unit
    = "ml_image_border_fill" "ml_image_border_fill_native"
    
external do_fill: t -> int -> int -> int -> unit 
    = "ml_image_fill"

external do_set_antialiased: t -> int -> unit
    = "ml_image_set_antialiased"
external do_set_antialiased_dont_blend: t -> int -> int -> unit
    = "ml_image_set_antialiased_dont_blend"

external do_set_brush: t -> t -> unit
    = "ml_image_set_brush"
external do_set_tile: t -> t -> unit
    = "ml_image_set_tile"
external do_set_thickness: t -> int -> unit
    = "ml_image_set_thickness"
external do_set_clip: t -> int -> int -> int -> int -> unit
    = "ml_image_set_clip"

external do_save_png: t -> string -> unit = "ml_save_png"
external do_save_jpeg: t -> string -> int -> unit = "ml_save_jpeg"

external do_dump_png: t -> out_channel -> unit = "ml_dump_png"
external do_dump_jpeg: t -> out_channel -> int -> unit = "ml_dump_jpeg"


(* External functions related to colors *)
external do_color_create: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_alloc"

external do_find_closest: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_closest"

external do_find_closest_hwb: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_closest_hwb"

external do_find_exact: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_exact"

external do_resolve: t -> red:int -> green:int -> blue:int -> c
    = "ml_image_color_resolve"

external do_green_channel: t -> int -> int = "ml_image_green_channel"
external do_red_channel: t -> int -> int = "ml_image_red_channel"
external do_blue_channel: t -> int -> int = "ml_image_blue_channel"
external do_alpha_channel: t -> int -> int = "ml_image_alpha_channel"
external do_get_transparent: t -> int = "ml_image_get_transparent"
external do_set_transparent: t -> int -> unit = "ml_image_set_transparent"

external do_get_font: int -> font = "ml_get_font"

external do_draw_char: t -> font -> int -> int -> char -> int -> unit
    = "ml_image_char" "ml_image_char_native"

external do_draw_charu: t -> font -> int -> int -> char -> int -> unit
    = "ml_image_charu" "ml_image_charu_native"

external do_draw_str: t -> font -> int -> int -> string -> int -> unit
    = "ml_image_str" "ml_image_str_native"

external do_draw_stru: t -> font -> int -> int -> string -> int -> unit
    = "ml_image_stru" "ml_image_stru_native"

external do_draw_str_ft:
    t -> int -> string -> float -> float -> int -> int -> string -> int array
    = "ml_image_str_ft" "ml_image_str_ft_native"
external do_draw_str_ftex:
    t -> int -> string -> float -> float -> int -> int -> ftex_flag array ->
        float -> ftex_charmap -> string -> int array
    = "ml_image_str_ftex" "ml_image_str_ftex_native"

external do_ft_bbox:
    string -> float -> float -> int -> int -> string -> int array
    = "ml_image_ft_bbox" "ml_image_ft_bbox_native"
external do_ftex_bbox:
    string -> float -> float -> int -> int -> ftex_flag array -> 
        float -> ftex_charmap -> string -> int array
    = "ml_image_ftex_bbox" "ml_image_ftex_bbox_native"

external do_copy: t -> t -> x:int -> y:int -> src_x:int -> src_y:int ->
    w:int -> h:int -> unit
    = "ml_image_copy" "ml_image_copy_native"
external do_copy_resized: t -> t -> x:int -> y:int -> src_x:int -> 
    src_y:int -> w:int -> h:int -> src_w:int -> src_h:int -> unit
    = "ml_image_copy_resized" "ml_image_copy_resized_native"
external do_copy_resampled: t -> t -> x:int -> y:int -> src_x:int -> 
    src_y:int -> w:int -> h:int -> src_w:int -> src_h:int -> unit
    = "ml_image_copy_resampled" "ml_image_copy_resampled_native"
external do_copy_rotated: t -> t -> x:float -> y:float -> src_x:int ->
    src_y:int -> w:int -> h:int -> angle:int -> unit
    = "ml_image_copy_rotated" "ml_image_copy_rotated_native"
external do_copy_merge: t -> t -> x:int -> y:int -> src_x:int ->
    src_y:int -> w:int -> h:int -> pct:int -> unit
    = "ml_image_copy_merge" "ml_image_copy_merge_native"
external do_copy_merge_gray: t -> t -> x:int -> y:int -> src_x:int ->
    src_y:int -> w:int -> h:int -> pct:int -> unit
    = "ml_image_copy_merge_gray" "ml_image_copy_merge_gray_native"
external do_palette_copy: t -> t -> unit 
    = "ml_image_palette_copy"


module Font =
  struct
    let tiny = do_get_font 0
    let small = do_get_font 1
    let medium = do_get_font 2
    let large = do_get_font 3
    let giant = do_get_font 4
  end


(* Implementation classes *)
(*
class gdColor im col =
  object(self)
    inherit color
    val antialias_color = false
    method code = col
    method blue_part = do_blue_part im col
    method red_part = do_red_part im col
    method green_part = do_green_part im col
    method antialiased = ({< antialias_color = true >} :> color)
    method is_aa = antialias_color
  end
*)

      
class virtual gd_color_allocator im = 
  object (self)
    inherit color_allocator
  
    val mutable aa_pcolor = false
    val mutable brushed_pcolor = false
    val mutable styled_pcolor = false
    val mutable styled_brushed_pcolor = false
    val mutable tiled_pcolor = false
    val mutable transparent_pcolor = true

      
    method create ~red ~green ~blue =
      let cindex = do_color_create im ~red ~green ~blue in
      if cindex = -1 then raise Too_many_colors 
      else self#new_ml_color cindex
  
    method closest ~red ~green ~blue =
      let cindex = do_find_closest im ~red ~green ~blue in
      if cindex = -1 then raise Color_not_found 
      else self#new_ml_color cindex
  
    method closest_hwb ~red ~green ~blue =
      let cindex = do_find_closest_hwb im ~red ~green ~blue in
      if cindex = -1 then raise Color_not_found 
      else self#new_ml_color cindex
  
    method exact ~red ~green ~blue =
      let cindex = do_find_exact im ~red ~green ~blue in
      if cindex = -1 then raise Color_not_found 
      else self#new_ml_color cindex
  
    method resolve ~red ~green ~blue =
      let cindex = do_resolve im ~red ~green ~blue in
      if cindex = -1 then raise Color_not_found 
      else self#new_ml_color cindex
  
    method find ~red ~green ~blue =
      let cindex = do_find_exact im ~red ~green ~blue in
      if cindex <> -1 then
        self#new_ml_color cindex
      else
        let cindex = do_color_create im ~red ~blue ~green in
        if cindex = -1 then raise Too_many_colors 
        else self#new_ml_color cindex

    method black = self#find ~red:0 ~blue:0 ~green:0
    method white = self#find ~red:255 ~blue:255 ~green:255
    method blue = self#find ~blue:255 ~red:0 ~green:0
    method green = self#find ~green:255 ~red:0 ~blue:0
    method red = self#find ~red:255 ~green:0 ~blue:0
    method get_transparent =
      let cindex = do_get_transparent im in
      if cindex = -1 then raise Color_not_found 
      else self#new_ml_color cindex
    method set_transparent color =
      do_set_transparent im color.index
    method set_antialiased enable = aa_pcolor <- enable 
    method set_brushed enable =
      brushed_pcolor <- enable;
      styled_brushed_pcolor <- enable && styled_pcolor
    method set_styled enable =
      styled_pcolor <- enable;
      styled_brushed_pcolor <- enable && brushed_pcolor
    method set_tiled enable = tiled_pcolor <- enable
    method antialiased () = 
      if aa_pcolor then 0
      else raise (Illegal_state 
        "You must call 'set_antialiased' before calling 'antialiased'.")
    method brushed () = 
      if brushed_pcolor then 1
      else raise (Illegal_state 
        "You must call 'set_brushed' before calling 'brushed'.")
    method styled () = 
      if styled_pcolor then 2
      else raise (Illegal_state 
        "You must call 'set_styled' before calling 'styled'.")
    method styled_brushed () = 
      if styled_brushed_pcolor then 3
      else raise (Illegal_state 
        "You must call 'set_brushed' and 'set_styled' before calling 
         'styled_brushed'.")
    method tiled () = 
      if tiled_pcolor then 4
      else raise (Illegal_state 
        "You must call 'set_tiled' before calling 'tiled'.")
    method transparent () =
      if transparent_pcolor then 5
      else raise (Illegal_state 
        "Transparent pseudocolor is disabled.")
  end

class gd_8bit_color_allocator im =
  object(self)
    inherit gd_color_allocator im

    val colors = Array.make 256 
      { index = -1; red_channel = -1; green_channel = -1;
        blue_channel = -1; alpha_channel = -1; } 

    method private new_ml_color idx =
      let mc =
        { index = idx;
          red_channel = (do_red_channel im idx);
          green_channel = (do_green_channel im idx);
          blue_channel = (do_blue_channel im idx);
          alpha_channel = (do_alpha_channel im idx); }  in
      colors.(idx) <- mc;
      mc

    method get_color_by_index idx =
      let c = colors.(idx) in
      if c.index = -1 then self#new_ml_color idx
      else c
  
  end

class gd_truecolor_allocator im =
  object(self)
    inherit gd_color_allocator im

    val colors:((int, color) Hashtbl.t) = Hashtbl.create 1024

    method private new_ml_color idx =
      let mc =
        { index = idx;
          red_channel = (do_red_channel im idx);
          green_channel = (do_green_channel im idx);
          blue_channel = (do_blue_channel im idx);
          alpha_channel = (do_alpha_channel im idx); }  in
      Hashtbl.replace colors idx mc;
      mc

    method get_color_by_index idx =
      try
        Hashtbl.find colors idx
      with Not_found ->
        self#new_ml_color idx
  
  end

      

class virtual gdImage im =
  object(self)
    inherit image
  
    method get_image = im
  
    method line ~x1 ~y1 ~x2 ~y2 ?(pseudo = -1) color =
      do_draw_line im x1 y1 x2 y2 color.index pseudo
  
    method dashed_line ~x1 ~y1 ~x2 ~y2 ?(pseudo = -1) color =
      do_draw_dline im x1 y1 x2 y2 color.index pseudo
  
    method rectangle ~x1 ~y1 ~x2 ~y2 ?(pseudo = -1) color =
      do_draw_rect im x1 y1 x2 y2 color.index pseudo
  
    method filled_rectangle ~x1 ~y1 ~x2 ~y2 ?(pseudo = -1) color =
      do_draw_frect im x1 y1 x2 y2 color.index pseudo
  
    method polygon ~pts ?(pseudo = -1) color =
      do_draw_poly im pts (Array.length pts) color.index pseudo
  
    method filled_polygon ~pts ?(pseudo = -1) color =
      do_draw_fpoly im pts (Array.length pts) color.index pseudo
  
    method arc ~cx ~cy ~w ~h ~s ~e ?(pseudo = -1) color =
      do_draw_arc im cx cy w h s e color.index pseudo
  
    method closed_arc
        ~cx ~cy ~w ~h ~s ~e ?(nofill = false) ?(edged = false) ?(pseudo = -1) color =
      do_draw_carc im cx cy w h s e color.index pseudo nofill edged
  
    method closed_chord
        ~cx ~cy ~w ~h ~s ~e ?(nofill = false) ?(edged = false) ?(pseudo = -1) color =
      do_draw_cchord im cx cy w h s e color.index pseudo nofill edged
  
    method filled_ellipse ~cx ~cy ~w ~h ?(pseudo = -1) color =
      do_draw_fell im cx cy w h color.index pseudo
  
    method border_fill ~x ~y ~border ~fill =
      do_border_fill im x y (border.index) (fill.index)
  
    method fill ~x ~y color =
      do_fill im x y color.index
  
    method set_antialiased col =
      self#colors#set_antialiased true;
      do_set_antialiased im col.index
  
    method set_antialiased_dont_blend ~aacolor ~dontblend =
      self#colors#set_antialiased true;
      do_set_antialiased_dont_blend im aacolor.index dontblend.index
  
    method set_brush br =
      self#colors#set_brushed true;
      do_set_brush im br#get_image
    method set_tile ti =
      self#colors#set_tiled true;
      do_set_tile im ti#get_image
    method set_thickness th = do_set_thickness im th
    method set_clip ~x1 ~y1 ~x2 ~y2 =
      do_set_clip im x1 y1 x2 y2
  
    method letter ~font ~x ~y ~c color =
      do_draw_char im font x y c color.index
  
    method letter_up ~font ~x ~y ~c color =
      do_draw_charu im font x y c color.index
  
    method string ~font ~x ~y ~s color =
      do_draw_str im font x y s color.index
  
    method string_up ~font ~x ~y ~s color =
      do_draw_stru im font x y s color.index

    method string_ft ~fg ~fname ~size ~angle ~x ~y text =
      do_draw_str_ft im fg.index fname size angle x y text
    method string_ftex ~fg ~fname ~size ~angle ~x ~y ?(flags = [||]) 
        ?(spacing = 1.05) ?(charmap = FTExUnicode) text =
      do_draw_str_ftex im fg.index fname size angle x y flags spacing 
        charmap text
  
    method save_as_png filename = do_save_png im filename
    method save_as_jpeg ?(quality = -1) filename =
      do_save_jpeg im filename quality
  
    method out_as_png channel = do_dump_png im channel
    method out_as_jpeg ?(quality = -1) channel =
      do_dump_jpeg im channel quality
  
    method set_pixel ~x ~y color =
      do_set_pixel im x y color.index
  
    method get_pixel ~x ~y =
      self#colors#get_color_by_index (do_get_pixel im x y)
  
    method width = do_get_width im
    method height = do_get_height im
  
    method in_range ~x ~y =
      x >= 0 && x <= (do_get_width im) && y >= 0 && y <= (do_get_height im)

    method copy src ~x ~y ~src_x ~src_y ~w ~h =
      do_copy im src#get_image x y src_x src_y w h
    method copy_resized src ~x ~y ~src_x ~src_y ~w ~h ~src_w ~src_h =
      do_copy_resized im src#get_image x y src_x src_y w h src_w src_h
    method copy_resampled src ~x ~y ~src_x ~src_y ~w ~h ~src_w ~src_h =
      do_copy_resampled im src#get_image x y src_x src_y w h src_w src_h
    method copy_rotated src ~x ~y ~src_x ~src_y ~w ~h ~angle =
      do_copy_rotated im src#get_image x y src_x src_y w h angle
    method copy_merge src ~x ~y ~src_x ~src_y ~w ~h ~pct =
      do_copy_merge im src#get_image x y src_x src_y w h pct
    method copy_merge_gray src ~x ~y ~src_x ~src_y ~w ~h ~pct =
      do_copy_merge_gray im src#get_image x y src_x src_y w h pct
    method palette_copy src = 
      do_palette_copy im src#get_image

  end

(* 8-bit (indexed-color) image *)
class gdImage8 im =
  object
    inherit gdImage im

    val c_a = new gd_8bit_color_allocator im
    method colors = c_a
  end

(* Truecolor image *)
class gdImageT im =
  object
    inherit gdImage im

    val c_a = new gd_truecolor_allocator im
    method colors = c_a
  end


let is_truecolor im = do_is_truecolor im

let ft_bbox ~fname ~size ~angle ~x ~y text =
  do_ft_bbox fname size angle x y text
let ftex_bbox ~fname ~size ~angle ~x ~y ?(flags = [||]) ?(spacing = 1.05)
    ?(charmap = FTExUnicode) text =
  do_ftex_bbox fname size angle x y flags spacing charmap text

(* Image creation functions *)
let create ~(x:int) ~(y:int) = 
  new gdImage8 (do_image_create x y)

let create_truecolor ~(x:int) ~(y:int) = 
  new gdImageT (do_image_create_truecolor x y)

let open_png filename =
  let im = (do_image_open_png filename) in
  if (is_truecolor im) then new gdImageT im
  else new gdImage8 im

let open_jpeg filename =
  new gdImageT (do_image_open_jpeg filename)
