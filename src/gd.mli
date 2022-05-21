(* $Header: /home/cvs/gd4o/gd.mli,v 1.6 2003/11/25 01:02:32 matt Exp $ *)
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

type t (* image type *)

(* GD images can only have 256 colors *)
exception Too_many_colors

(* Tried to find a preallocated color that wasn't *)
exception Color_not_found

(* Couldn't create or open an image. *)
exception Image_creation_failed

(* This build doesn't support some image format (Jpeg, Xpm, etc.) *)
exception Not_supported

(* An operation was attempted without having performed one or more prerequisites. *)
exception Illegal_state of string

(* All of these classes are virtual because users shouldn't be instatiating them.
   Instead, use the creation or opening functions listed at the bottom *)

(*
class virtual color :
object
  (* Returns the red part (0-255) of the color *)
  method virtual red_part : int
    (* Returns the green part (0-255) of the color *)
  method virtual green_part : int
    (* Returns the blue part (0-255) of the color *)
  method virtual blue_part : int
    (* Returns the code of the color. Please don't use. *)
  method virtual code : int
  method virtual antialiased : color
  method virtual is_aa : bool
end
*)

type color
type ftex_flag = FTExSetSpacing | FTExSetCharmap
type ftex_charmap = FTExUnicode | FTExShiftJIS | FTExBig5

(* The first color allocated for an image is it's background *)
class virtual color_allocator :
  object
    (* R, G, and B values are integers 0-255 *)
    method virtual create : red:int -> green:int -> blue:int -> color

    (* Return the closest-matching color of those already allocated *)
    method virtual closest : red:int -> green:int -> blue:int -> color
    method virtual closest_hwb : red:int -> green:int -> blue:int -> color

    (* Try exact, create, closest *)
    method virtual resolve : red:int -> green:int -> blue:int -> color

    (* Exact match color of those already allocated *)
    method virtual exact : red:int -> green:int -> blue:int -> color

    (* Try an exact, create *)
    method virtual find : red:int -> green:int -> blue:int -> color
    method virtual get_color_by_index : int -> color
    method virtual new_ml_color : int -> color
    method virtual white : color
    method virtual black : color
    method virtual blue : color
    method virtual green : color
    method virtual red : color
    method virtual get_transparent : color
    method virtual set_transparent : color -> unit
    method virtual set_antialiased : bool -> unit
    method virtual set_brushed : bool -> unit
    method virtual set_styled : bool -> unit
    method virtual set_tiled : bool -> unit
    method virtual antialiased : unit -> int
    method virtual brushed : unit -> int
    method virtual styled : unit -> int
    method virtual styled_brushed : unit -> int
    method virtual tiled : unit -> int
    method virtual transparent : unit -> int
  end

type font

module Font : sig
  val tiny : font
  val small : font
  val medium : font
  val large : font
  val giant : font
end

class virtual image :
  object
    (* This was private, but it needs to be exposed for things like tile
       brush images. *)
    method virtual get_image : t

    (* Return the color_allocator object associated with this image *)
    method virtual colors : color_allocator

    method virtual line :
      x1:int -> y1:int -> x2:int -> y2:int -> ?pseudo:int -> color -> unit

    method virtual dashed_line :
      x1:int -> y1:int -> x2:int -> y2:int -> ?pseudo:int -> color -> unit

    method virtual rectangle :
      x1:int -> y1:int -> x2:int -> y2:int -> ?pseudo:int -> color -> unit

    method virtual filled_rectangle :
      x1:int -> y1:int -> x2:int -> y2:int -> ?pseudo:int -> color -> unit

    method virtual polygon :
      pts:(int * int) array -> ?pseudo:int -> color -> unit

    method virtual filled_polygon :
      pts:(int * int) array -> ?pseudo:int -> color -> unit

    method virtual arc :
      cx:int ->
      cy:int ->
      w:int ->
      h:int ->
      s:int ->
      e:int ->
      ?pseudo:int ->
      color ->
      unit

    method virtual closed_arc :
      cx:int ->
      cy:int ->
      w:int ->
      h:int ->
      s:int ->
      e:int ->
      ?nofill:bool ->
      ?edged:bool ->
      ?pseudo:int ->
      color ->
      unit

    method virtual closed_chord :
      cx:int ->
      cy:int ->
      w:int ->
      h:int ->
      s:int ->
      e:int ->
      ?nofill:bool ->
      ?edged:bool ->
      ?pseudo:int ->
      color ->
      unit

    method virtual filled_ellipse :
      cx:int -> cy:int -> w:int -> h:int -> ?pseudo:int -> color -> unit

    (* Fill an area bordered by the border color *)
    method virtual border_fill :
      x:int -> y:int -> border:color -> fill:color -> unit

    (* Fill an area with the same color as the pixel  *)
    method virtual fill : x:int -> y:int -> color -> unit

    (* Turn on antialiasing. *)
    method virtual set_antialiased : color -> unit

    method virtual set_antialiased_dont_blend :
      aacolor:color -> dontblend:color -> unit

    method virtual set_brush : image -> unit
    method virtual set_tile : image -> unit
    method virtual set_thickness : int -> unit
    method virtual set_clip : x1:int -> y1:int -> x2:int -> y2:int -> unit

    (* Draw one character *)
    method virtual letter :
      font:font -> x:int -> y:int -> c:char -> color -> unit

    (* Rotated 90 degrees *)
    method virtual letter_up :
      font:font -> x:int -> y:int -> c:char -> color -> unit

    method virtual string :
      font:font -> x:int -> y:int -> s:string -> color -> unit

    (* Rotated 90 degrees *)
    method virtual string_up :
      font:font -> x:int -> y:int -> s:string -> color -> unit

    (* Freetype string *)
    method virtual string_ft :
      fg:color ->
      fname:string ->
      size:float ->
      angle:float ->
      x:int ->
      y:int ->
      string ->
      int array

    method virtual string_ftex :
      fg:color ->
      fname:string ->
      size:float ->
      angle:float ->
      x:int ->
      y:int ->
      ?flags:ftex_flag array ->
      ?spacing:float ->
      ?charmap:ftex_charmap ->
      string ->
      int array

    method virtual set_pixel : x:int -> y:int -> color -> unit
    method virtual get_pixel : x:int -> y:int -> color

    (* Image's size *)
    method virtual width : int
    method virtual height : int

    (* Is proposed drawing location within the drawing area? *)
    method virtual in_range : x:int -> y:int -> bool

    (* Save to file *)
    method virtual save_as_png : string -> unit
    method virtual save_as_jpeg : ?quality:int -> string -> unit

    (* Dump to an out_channel. *)
    method virtual out_as_png : out_channel -> unit
    method virtual out_as_jpeg : ?quality:int -> out_channel -> unit

    (* Copy a region from another image to this image. *)
    method virtual copy :
      image ->
      x:int ->
      y:int ->
      src_x:int ->
      src_y:int ->
      w:int ->
      h:int ->
      unit

    method virtual copy_resized :
      image ->
      x:int ->
      y:int ->
      src_x:int ->
      src_y:int ->
      w:int ->
      h:int ->
      src_w:int ->
      src_h:int ->
      unit

    method virtual copy_resampled :
      image ->
      x:int ->
      y:int ->
      src_x:int ->
      src_y:int ->
      w:int ->
      h:int ->
      src_w:int ->
      src_h:int ->
      unit

    method virtual copy_rotated :
      image ->
      x:float ->
      y:float ->
      src_x:int ->
      src_y:int ->
      w:int ->
      h:int ->
      angle:int ->
      unit

    method virtual copy_merge :
      image ->
      x:int ->
      y:int ->
      src_x:int ->
      src_y:int ->
      w:int ->
      h:int ->
      pct:int ->
      unit

    method virtual copy_merge_gray :
      image ->
      x:int ->
      y:int ->
      src_x:int ->
      src_y:int ->
      w:int ->
      h:int ->
      pct:int ->
      unit

    method virtual palette_copy : image -> unit
  end

(* Create a new image with the given size *)
val create : x:int -> y:int -> image (* Throws Gd.Image_creation_faield *)

(* Create a new image with the given size *)
val create_truecolor : x:int -> y:int -> image
(* Throws Gd.Image_creation_faield *)

(* Open a png file. Throws Not_found and Gd.Image_creation_failed *)
val open_png : string -> image

(* Same, but for jpeg's *)
val open_jpeg : string -> image
val is_truecolor : t -> bool

(* Return the bounding box for a string rendered with FreeType *)
val ft_bbox :
  fname:string ->
  size:float ->
  angle:float ->
  x:int ->
  y:int ->
  string ->
  int array

val ftex_bbox :
  fname:string ->
  size:float ->
  angle:float ->
  x:int ->
  y:int ->
  ?flags:ftex_flag array ->
  ?spacing:float ->
  ?charmap:ftex_charmap ->
  string ->
  int array
