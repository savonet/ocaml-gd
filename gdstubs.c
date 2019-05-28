/* $Header: /home/cvs/gd4o/gdstubs.c,v 1.7 2003/11/25 01:02:32 matt Exp $ */
/*
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
 */



#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/callback.h>
#include <caml/fail.h>

#include <gd.h>
#include <gdfontl.h>
#include <gdfonts.h>
#include <gdfontt.h>
#include <gdfontmb.h>
#include <gdfontg.h>

struct gd_wrapper {
  gdImagePtr im;
};

typedef struct gd_wrapper GdWrapper;

struct font_wrapper {
  gdFontPtr font;
};

typedef struct font_wrapper GdFWrapper;

struct points_wrapper {
  gdPointPtr pts;
};

typedef struct points_wrapper GdPtsWrapper;

#define IM_VAL(X) ((*((GdWrapper *)(Data_custom_val(X)))).im)
#define FONT_VAL(X) ((*((GdFWrapper *)(Data_custom_val(X)))).font)
#define PTS_VAL(X) ((*((GdPtsWrapper *)(Data_custom_val(X)))).pts)

static void ml_gd_finalize(value);
static long ml_gd_hash(value);
static int ml_font_cmp(value, value);
static long ml_font_hash(value);

static struct custom_operations image_t_custom_operations = {
  "GD image/0.1",
  ml_gd_finalize,
  NULL,
  ml_gd_hash,
  NULL,
  NULL,
  NULL
};

static struct custom_operations font_t_custom_operations = {
  "GD font/0.1",
  NULL,
  ml_font_cmp,
  ml_font_hash,
  NULL,
  NULL,
  NULL
};

static gdFontPtr fonts[5] =
{
  NULL,
  NULL,
  NULL,
  NULL,
  NULL
};

static int fonts_init = 0;

static int pseudoColors[6] =
    { gdAntiAliased, gdBrushed, gdStyled,
      gdStyledBrushed, gdTiled, gdTransparent };
static int ftExFlags[2] =
    { gdFTEX_LINESPACE, gdFTEX_CHARMAP };
static int ftExCharmaps[3] =
    { gdFTEX_Unicode, gdFTEX_Shift_JIS, gdFTEX_Big5 };

void ml_gd_finalize(value v) {
  if (IM_VAL(v))
    gdImageDestroy(IM_VAL(v));
}

long ml_gd_hash(value v) {
  return gdImageSX(IM_VAL(v));
}

int ml_font_cmp(value v1, value v2) {
  return (long)FONT_VAL(v1) - (long)FONT_VAL(v2);
}

static long ml_font_hash(value v) {
  return (long)FONT_VAL(v);
}

value ml_get_font(value i) {
  CAMLparam1(i);  
  CAMLlocal1(v);

  v = alloc_custom(&font_t_custom_operations, sizeof(GdFWrapper), 1, 10);

  if (!fonts_init) {
    fonts[0] = gdFontTiny;
    fonts[1] = gdFontSmall;
    fonts[2] = gdFontMediumBold;
    fonts[3] = gdFontLarge;
    fonts[4] = gdFontGiant;
    fonts_init = 1;
  }

  FONT_VAL(v) = fonts[Int_val(i)];

  CAMLreturn(v);
}

value ml_image_create(value sx, value sy) {
  CAMLparam2(sx, sy);
  CAMLlocal1(v);
  gdImagePtr im;

  im = gdImageCreate(Int_val(sx), Int_val(sy));
  if (!im) 
    raise_constant(*(value *)caml_named_value("gdopen failed"));

  v = alloc_custom(&image_t_custom_operations, sizeof(GdWrapper),
                   (Int_val(sx) * Int_val(sy)) + sizeof(gdImage), 10000);
  IM_VAL(v) = im;

  CAMLreturn(v);
}

value ml_image_create_truecolor(value sx, value sy) {
  CAMLparam2(sx, sy);
  CAMLlocal1(v);
  gdImagePtr im;

  im = gdImageCreateTrueColor(Int_val(sx), Int_val(sy));
  if (!im) 
    raise_constant(*(value *)caml_named_value("gdopen failed"));

  v = alloc_custom(&image_t_custom_operations, sizeof(GdWrapper),
                   (Int_val(sx) * Int_val(sy)) + sizeof(gdImage), 10000);
  IM_VAL(v) = im;

  CAMLreturn(v);
}

value ml_image_open_png(value filename) {
  CAMLparam1(filename);
  CAMLlocal1(v);
  FILE *in;
  gdImagePtr im;

  in = fopen(String_val(filename), "rb");
  if (!in)
    raise_not_found();
  
  im = gdImageCreateFromPng(in);

  fclose(in);

  if (!im) 
    raise_constant(*(value *)caml_named_value("gdopen failed"));

  v =  alloc_custom(&image_t_custom_operations, sizeof(GdWrapper),
                    sizeof(gdImage) + (gdImageSX(im) * gdImageSY(im)), 100000);
  IM_VAL(v) = im;

  CAMLreturn(v);
}

/* This is useful when an image has been created from a PNG file,
 * so it could be either truecolor or 8-bit. */
value ml_image_is_truecolor(value gdw) {
  gdImagePtr im;
  im = IM_VAL(gdw);
  if (im->trueColor) {
      return Val_true;
  }
  else {
      return Val_false;
  }
}

value ml_image_open_jpeg(value filename) {
#ifdef HAVE_JPEG
  FILE *in;
  gdImagePtr im;
  CAMLparam1(filename);
  CAMLlocal1(v);

  in = fopen(String_val(filename), "rb");
  if (!in)
    raise_not_found();
  
  im = gdImageCreateFromJpeg(in);

  fclose(in);

  if (!im) 
    raise_constant(*(value *)caml_named_value("gdopen failed"));

  v =  alloc_custom(&image_t_custom_operations, sizeof(GdWrapper),
                    sizeof(gdImage) + (gdImageSX(im) * gdImageSY(im)), 100000);
  IM_VAL(v) = im;
  CAMLreturn(v);
#else
  raise_constant(*(value *)caml_named_value("gd type not supported"));
  return Val_unit;
#endif
}

value ml_image_line_native(value gdw, value x1, value y1, value x2, value y2,
                           value c, value pc) {
  int pcval;
  pcval = Int_val(pc);

  if (pcval >= 0)
    gdImageLine(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2), Int_val(y2),
                pseudoColors[pcval]);
  else
    gdImageLine(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2), Int_val(y2),
                Int_val(c));
  return Val_unit;
}

value ml_image_line(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 7);
#endif
  return
    ml_image_line_native(argv[0], argv[1], argv[2], argv[3], argv[4], 
                         argv[5], argv[6]);
}

value ml_image_dline_native(value gdw, value x1, value y1, value x2, value y2,
                            value c, value pc) {
  int pcval;
  pcval = Int_val(pc);

  if (pcval >= 0)
    gdImageDashedLine(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                      Int_val(y2), pseudoColors[pcval]);
  else
    gdImageDashedLine(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                      Int_val(y2), Int_val(c));
  return Val_unit;
}

value ml_image_dline(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 7);
#endif
  return
    ml_image_line_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                         argv[5], argv[6]);
}

value ml_image_rect_native(value gdw, value x1, value y1, value x2, value y2,
                           value c, value pc) {
  int pcval;
  pcval = Int_val(pc);

  if (pcval >= 0) 
    gdImageRectangle(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                     Int_val(y2), pseudoColors[pcval]);
  else
    gdImageRectangle(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                     Int_val(y2), Int_val(c));
  return Val_unit;
}

value ml_image_rect(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 7);
#endif
  return
    ml_image_rect_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                         argv[5], argv[6]);
}

value ml_image_frect_native(value gdw, value x1, value y1, value x2, value y2,
                            value c, value pc) {
  int pcval;
  pcval = Int_val(pc);

  if (pcval >= 0)
    gdImageFilledRectangle(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                           Int_val(y2), pseudoColors[pcval]);
  else
    gdImageFilledRectangle(IM_VAL(gdw), Int_val(x1), Int_val(y1), Int_val(x2),
                           Int_val(y2), Int_val(c));
  return Val_unit;
}

value ml_image_frect(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 7);
#endif
  return
    ml_image_frect_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                          argv[5], argv[6]);
}

/* Draw a polygon. */
value ml_image_poly(value gdw, value points, value numpts, value c, value pc) {
  gdPoint gd_points[numpts];
  int i, n, pcval;
  pcval = Int_val(pc);

  n = Int_val(numpts);
  for (i = 0; i < n; i++) {
    gd_points[i].x = Int_val(Field(Field(points, i), 0));
    gd_points[i].y = Int_val(Field(Field(points, i), 1));
  }
  
  if (pcval >= 0)
    gdImagePolygon(IM_VAL(gdw), gd_points, Int_val(numpts), pseudoColors[pcval]);
  else
    gdImagePolygon(IM_VAL(gdw), gd_points, Int_val(numpts), Int_val(c));
  
  return Val_unit;
}

/* Draw a filled polygon. */
value ml_image_fpoly(value gdw, value points, value numpts, value c, value pc) {
  gdPoint gd_points[numpts];
  int i, n, pcval;
  pcval = Int_val(pc);

  n = Int_val(numpts);
  for (i = 0; i < n; i++) {
    gd_points[i].x = Int_val(Field(Field(points, i), 0));
    gd_points[i].y = Int_val(Field(Field(points, i), 1));
  }
  
  if (pcval >= 0)
    gdImageFilledPolygon(IM_VAL(gdw), gd_points, Int_val(numpts), pseudoColors[pcval]);
  else
    gdImageFilledPolygon(IM_VAL(gdw), gd_points, Int_val(numpts), Int_val(c));
  
  return Val_unit;
}

value ml_image_fell_native(value gdw, value cx, value cy, value w, value h,
                           value c, value pc) {
  int pcval;
  pcval = Int_val(pc);

  if (pcval >= 0)
    gdImageFilledEllipse(IM_VAL(gdw), Int_val(cx), Int_val(cy), Int_val(w),
                         Int_val(h), pseudoColors[pcval]);
  else
    gdImageFilledEllipse(IM_VAL(gdw), Int_val(cx), Int_val(cy), Int_val(w),
                         Int_val(h), Int_val(c));
  return Val_unit;
}

value ml_image_fell(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 7);
#endif
  return
    ml_image_fell_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                         argv[5], argv[6]);
}

value ml_image_arc_native(value gdw, value cx, value cy, value w, value h,
                          value s, value e, value c, value pc) {
  int pcval;
  pcval = Int_val(pc);

  if (pcval >= 0)
    gdImageArc(IM_VAL(gdw), Int_val(cx), Int_val(cy), Int_val(w), Int_val(h),
               Int_val(s), Int_val(e), pseudoColors[pcval]);
  else
    gdImageArc(IM_VAL(gdw), Int_val(cx), Int_val(cy), Int_val(w), Int_val(h),
               Int_val(s), Int_val(e), Int_val(c));
  return Val_unit;
}

value ml_image_arc(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 9);
#endif
  return
    ml_image_arc_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                        argv[6], argv[7], argv[8]);
}

value ml_image_carc_native(value gdw, value cx, value cy, value w,
                           value h, value s, value e, value c,
                           value pc, value nofill, value edged) {
  int color, style, pcval;
  pcval = Int_val(pc);

  if (pcval >= 0) color = pseudoColors[pcval];
  else color = Int_val(c);
  style = gdArc;
  if (Bool_val(nofill)) style |= gdNoFill;
  if (Bool_val(edged)) style |= gdEdged;

  gdImageFilledArc(IM_VAL(gdw), Int_val(cx), Int_val(cy), Int_val(w),
                   Int_val(h), Int_val(s), Int_val(e), color, style);

  return Val_unit;
}

value ml_image_carc(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 11);
#endif
  return
    ml_image_carc_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                         argv[6], argv[7], argv[8], argv[9], argv[10]);
}

value ml_image_cchord_native(value gdw, value cx, value cy, value w,
                             value h, value s, value e, value c,
                             value pc, value nofill, value edged) {
  int color, style, pcval;
  pcval = Int_val(pc);

  if (pcval >= 0) color = pseudoColors[pcval];
  else color = Int_val(c);
  style = gdChord;
  if (Bool_val(nofill)) style |= gdNoFill;
  if (Bool_val(edged)) style |= gdEdged;

  gdImageFilledArc(IM_VAL(gdw), Int_val(cx), Int_val(cy), Int_val(w),
                   Int_val(h), Int_val(s), Int_val(e), color, style);

  return Val_unit;
}

value ml_image_cchord(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 11);
#endif
  return
    ml_image_cchord_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
                           argv[6], argv[7], argv[8], argv[9], argv[10]);
}

value ml_image_border_fill_native(value gdw, value x, value y, value b,
                                  value c) {
  gdImageFillToBorder(IM_VAL(gdw), Int_val(x), Int_val(y), Int_val(b),
                      Int_val(c));
  return Val_unit;
}

value ml_image_border_fill(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 5);
#endif
  return
    ml_image_border_fill_native(argv[0], argv[1], argv[2], argv[3], argv[4]);
}

value ml_image_fill(value gdw, value x, value y, value c) {
     gdImageFill(IM_VAL(gdw), Int_val(x), Int_val(y), Int_val(c));
     return Val_unit;
}


value ml_image_char_native(value gdw, value font, value x, value y, value c,
                           value color) {
  gdImageChar(IM_VAL(gdw), FONT_VAL(font), Int_val(x), Int_val(y), Int_val(c),
              Int_val(color));
  return Val_unit;
}

value ml_image_char(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 6);
#endif
  return ml_image_char_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}

value ml_image_charu_native(value gdw, value font, value x, value y, value c,
                            value color) {
  gdImageCharUp(IM_VAL(gdw), FONT_VAL(font), Int_val(x), Int_val(y), Int_val(c),
                Int_val(color));
  return Val_unit;
}

value ml_image_charu(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 6);
#endif
  return ml_image_charu_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                               argv[5]);
}

value ml_image_str_native(value gdw, value font, value x, value y, value s,
                           value color) {
  gdImageString(IM_VAL(gdw), FONT_VAL(font), Int_val(x), Int_val(y),
                (unsigned char*)String_val(s), Int_val(color));
  return Val_unit;
}

value ml_image_str(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 6);
#endif
  return ml_image_str_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}

value ml_image_stru_native(value gdw, value font, value x, value y, value s,
                            value color) {
  gdImageStringUp(IM_VAL(gdw), FONT_VAL(font), Int_val(x), Int_val(y),
                  (unsigned char*)String_val(s), Int_val(color));
  return Val_unit;
}

value ml_image_stru(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 6);
#endif
  return ml_image_stru_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                              argv[5]);
}


void raise_freetype_exception(char *msg) {
  raise_with_string(*caml_named_value("gd freetype exception"), msg);
}


value ml_image_str_ft_base(gdImagePtr im, value fg, value fname, value size, 
                           value angle, value x, value y, value string) {

#ifdef HAVE_FREETYPE
  CAMLparam5(fg, fname, size, angle, x);
  CAMLxparam2(y, string);

  int brect[8];
  int i;
  char *rc;

  CAMLlocal1(ml_brect);
  ml_brect = alloc (8, 0);

  rc = gdImageStringFT(im, brect, Int_val(fg), String_val(fname),
                       Double_val(size), Double_val(angle), Int_val(x),
                       Int_val(y), String_val(string));

  if (rc != NULL) {
    raise_freetype_exception (rc);
  }

  for (i = 0; i < 8; i++) 
    Store_field(ml_brect, i, Val_int(brect[i]));
  
  CAMLreturn(ml_brect);
#else
  raise_constant(*(value *)caml_named_value("gd type not supported"));
  return Val_unit;
#endif
}

value ml_image_str_ft_native(value gdw, value fg, value fname, value size,
                             value angle, value x, value y, value string) {
  CAMLparam5(gdw, fg, fname, size, angle);
  CAMLxparam3(x, y, string);

  CAMLreturn(ml_image_str_ft_base(IM_VAL(gdw), fg, fname, size, angle,
                                  x, y, string));
}

value ml_image_str_ft(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 8);
#endif
  return ml_image_str_ft_native(argv[0], argv[1], argv[2], argv[3],
                                argv[4], argv[5], argv[6], argv[7]);
}

value ml_image_ft_bbox_native(value fname, value size, value angle, 
                              value x, value y, value string) {
  CAMLparam5(fname,  size, angle, x, y);
  CAMLxparam1(string);

  CAMLreturn(ml_image_str_ft_base(NULL, 0, fname, size, angle,
                                  x, y, string));
}


value ml_image_ft_bbox(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 6);
#endif
  return ml_image_ft_bbox_native(argv[0], argv[1], argv[2], argv[3],
                                 argv[4], argv[5]);
}


value ml_image_str_ftex_base(gdImagePtr im, value fg, value fname, value size, 
                             value angle, value x, value y, value flags,
                             value spacing, value charmap, value string) {
#ifdef HAVE_FREETYPE
  int numflags, i;
  int brect[8];
  char *rc;
  gdFTStringExtra extra;

  CAMLparam5(fg, fname, size, angle, x);
  CAMLxparam5(y, flags, spacing, charmap, string);
  CAMLlocal1(ml_brect);

  numflags = Wosize_val(flags);

  for (i = 0; i < numflags; i++)
      extra.flags |= ftExFlags[Int_val(Field(flags, i))];
  extra.linespacing = Double_val(spacing);
  extra.charmap = ftExCharmaps[Int_val(charmap)];

  ml_brect = alloc (8, 0);

  rc = gdImageStringFTEx(im, brect, Int_val(fg), String_val(fname),
                         Double_val(size), Double_val(angle), Int_val(x),
                         Int_val(y), String_val(string), &extra);

  if (rc != NULL) {
    raise_freetype_exception (rc);
  }

  for (i = 0; i < 8; i++) 
    Store_field(ml_brect, i, Val_int(brect[i]));
  
  CAMLreturn(ml_brect);
#else
  raise_constant(*(value *)caml_named_value("gd type not supported"));
  return Val_unit;
#endif
}

value ml_image_str_ftex_native(value gdw, value fg, value fname, value size,
                               value angle, value x, value y, value flags,
                               value spacing, value charmap, value string) {
  CAMLparam5(gdw, fg, fname, size, angle);
  CAMLxparam5(x, y, flags, spacing, charmap);
  CAMLxparam1(string);

  CAMLreturn(ml_image_str_ftex_base(IM_VAL(gdw), fg, fname, size, angle,
                                    x, y, flags, spacing, charmap, string));
}

value ml_image_str_ftex(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 11);
#endif
  return ml_image_str_ftex_native(argv[0], argv[1], argv[2], argv[3],
                                  argv[4], argv[5], argv[6], argv[7],
                                  argv[8], argv[9], argv[10]);
}

value ml_image_ftex_bbox_native(value fname, value size, value angle, 
                                value x, value y, value flags, value spacing,
                                value charmap, value string) {
  CAMLparam5(fname,  size, angle, x, y);
  CAMLxparam4(flags, spacing, charmap, string);

  CAMLreturn(ml_image_str_ftex_base(NULL, 0, fname, size, angle,
                                    x, y, flags, spacing, charmap,
                                    string));
}


value ml_image_ftex_bbox(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 9);
#endif
  return ml_image_ftex_bbox_native(argv[0], argv[1], argv[2], argv[3],
                                   argv[4], argv[5], argv[6], argv[7],
                                   argv[8]);
}


value ml_set_pixel(value gdw, value x, value y, value c) {
  gdImageSetPixel(IM_VAL(gdw), Int_val(x), Int_val(y), Int_val(c));
  return Val_unit;
}

value ml_get_pixel(value gdw, value x, value y) {
  return Val_int(gdImageGetPixel(IM_VAL(gdw), Int_val(x), Int_val(y)));
}

value ml_get_width(value gdw) {
  return Val_int(gdImageSX(IM_VAL(gdw)));
}

value ml_get_height(value gdw) {
  return Val_int(gdImageSY(IM_VAL(gdw)));
}

value ml_save_png(value gdw, value filename) {
  FILE *out;
  
  out = fopen(String_val(filename), "wb");
  gdImagePng(IM_VAL(gdw), out);
  fclose(out);
  
  return Val_unit;
}

value ml_save_jpeg(value gdw, value filename, value quality) {
#ifdef HAVE_JPEG
  FILE *out;

  out = fopen(String_val(filename), "wb");
  gdImageJpeg(IM_VAL(gdw), out, Int_val(quality));
  fclose(out);
#else
  raise_constant(*(value*)caml_named_value("gd type not supported"));
#endif
  return Val_unit;
}

/* Taken from the ocaml source... */
struct channel;
void really_putblock (struct channel *, char *, long);

/* Extract a struct channel * from the heap object representing it */
#define Channel(v) (*((struct channel **) (Data_custom_val(v))))

value ml_dump_png(value gdw, value chan) {
  int size;
  void* dat;

  dat = gdImagePngPtr(IM_VAL(gdw), &size);
  really_putblock(Channel(chan), dat, size);
  free(dat);
  
  return Val_unit;
}

value ml_dump_jpeg(value gdw, value chan, value quality) {
#ifdef HAVE_JPEG
  int size;
  void* dat;

  dat = gdImageJpegPtr(IM_VAL(gdw), &size, Int_val(quality));
  really_putblock(Channel(chan), dat, size);
  free(dat);
  
#else
  raise_constant(*(value*)caml_named_value("gd type not supported"));
#endif
  return Val_unit;
}


value ml_image_set_antialiased(value gdw, value c) {
  gdImageSetAntiAliased(IM_VAL(gdw), Int_val(c));
  return Val_unit;
}

value ml_image_set_antialiased_dont_blend(value gdw, value aa, value db) {
  gdImageSetAntiAliasedDontBlend(IM_VAL(gdw), Int_val(aa), Int_val(db));
  return Val_unit;
}

value ml_image_set_brush(value gdw, value br) {
  gdImageSetBrush(IM_VAL(gdw), IM_VAL(br));
  return Val_unit;
}

value ml_image_set_tile(value gdw, value t) {
  gdImageSetTile(IM_VAL(gdw), IM_VAL(t));
  return Val_unit;
}

value ml_image_set_thickness(value gdw, value t) {
  gdImageSetThickness(IM_VAL(gdw), Int_val(t));
  return Val_unit;
}

value ml_image_set_clip(value gdw, value x1, value y1, value x2, value y2) {
  gdImageSetClip(IM_VAL(gdw), Int_val(x1), Int_val(y1),
                 Int_val(x2), Int_val(y2));

  return Val_unit;
}

value ml_image_color_alloc(value gdw, value r, value g, value b) {
  int color;

  color = gdImageColorAllocate(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
    
  return Val_int(color);
}

value ml_image_color_closest(value gdw, value r, value g, value b) {
  int color;

  color = gdImageColorClosest(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
    
  return Val_int(color);
}

value ml_image_color_closest_hwb(value gdw, value r, value g, value b) {
  int color;

  color =
    gdImageColorClosestHWB(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
  return Val_int(color);
}

value ml_image_color_exact(value gdw, value r, value g, value b) {
  int color;

  color = gdImageColorExact(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
    
  return Val_int(color);
}

value ml_image_color_resolve(value gdw, value r, value g, value b) {
  int color;

  color = gdImageColorResolve(IM_VAL(gdw), Int_val(r), Int_val(g), Int_val(b));
    
  return Val_int(color);
}

value ml_image_red_channel(value gdw, value c) {
  return Val_int(gdImageRed(IM_VAL(gdw), Int_val(c)));
}

value ml_image_green_channel(value gdw, value c) {
  return Val_int(gdImageGreen(IM_VAL(gdw), Int_val(c)));
}

value ml_image_blue_channel(value gdw, value c) {
  return Val_int(gdImageBlue(IM_VAL(gdw), Int_val(c)));
}

value ml_image_alpha_channel(value gdw, value c) {
  return Val_int(gdImageAlpha(IM_VAL(gdw), Int_val(c)));
}

value ml_image_get_transparent(value gdw) {
  return Val_int(gdImageGetTransparent(IM_VAL(gdw)));
}

value ml_image_set_transparent(value gdw, value c) {
  gdImageColorTransparent(IM_VAL(gdw), Int_val(c));
  return Val_unit;
}

/* ================================================================== */
/* ============ Image Copying and Resizing Functions ================ */
/* ================================================================== */

value ml_image_copy_native(value dst, value src, value dst_x, value dst_y,
                           value src_x, value src_y, value w, value h) {
  gdImageCopy(IM_VAL(dst), IM_VAL(src), Int_val(dst_x), Int_val(dst_y),
              Int_val(src_x), Int_val(src_y), Int_val(w), Int_val(h));
  return Val_unit;
}

value ml_image_copy(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 8);
#endif
  return
    ml_image_copy_native(argv[0], argv[1], argv[2], argv[3], argv[4],
                         argv[5], argv[6], argv[7]);
}


value ml_image_copy_resized_native(value dst, value src, value dst_x,
                                   value dst_y, value src_x, value src_y, 
                                   value dst_w, value dst_h, value src_w,
                                   value src_h) {
  gdImageCopyResized(IM_VAL(dst), IM_VAL(src), Int_val(dst_x), 
                     Int_val(dst_y), Int_val(src_x), Int_val(src_y),
                     Int_val(dst_w), Int_val(dst_h), Int_val(src_w), 
                     Int_val(src_h));
  return Val_unit;
}

value ml_image_copy_resized(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 10);
#endif
  return
    ml_image_copy_resized_native(argv[0], argv[1], argv[2], argv[3],
                                 argv[4], argv[5], argv[6], argv[7],
                                 argv[8], argv[9]);
}


value ml_image_copy_resampled_native(value dst, value src, value dst_x, 
                                     value dst_y, value src_x, value src_y,
                                     value dst_w, value dst_h, value src_w,
                                     value src_h) {
  gdImageCopyResampled(IM_VAL(dst), IM_VAL(src), Int_val(dst_x), 
                       Int_val(dst_y), Int_val(src_x), Int_val(src_y),
                       Int_val(dst_w), Int_val(dst_h), Int_val(src_w), 
                       Int_val(src_h));
  return Val_unit;
}

value ml_image_copy_resampled(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 10);
#endif
  return
    ml_image_copy_resampled_native(argv[0], argv[1], argv[2], argv[3],
                                 argv[4], argv[5], argv[6], argv[7],
                                 argv[8], argv[9]);
}


value ml_image_copy_rotated_native(value dst, value src, value dst_x, 
                                   value dst_y, value src_x, value src_y,
                                   value w, value h, value angle) {
  gdImageCopyRotated(IM_VAL(dst), IM_VAL(src), Double_val(dst_x), 
                     Double_val(dst_y), Int_val(src_x), Int_val(src_y),
                     Int_val(w), Int_val(h), Int_val(angle));
  return Val_unit;
}

value ml_image_copy_rotated(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 9);
#endif
  return
    ml_image_copy_rotated_native(argv[0], argv[1], argv[2], argv[3], 
                                 argv[4], argv[5], argv[6], argv[7],
                                 argv[8]);
}


value ml_image_copy_merge_native(value dst, value src, value dst_x,
                                 value dst_y, value src_x, value src_y, 
                                 value w, value h, value pct) {
  gdImageCopyMerge(IM_VAL(dst), IM_VAL(src), Int_val(dst_x), Int_val(dst_y),
                   Int_val(src_x), Int_val(src_y), Int_val(w), Int_val(h),
                   Int_val(pct));
  return Val_unit;
}

value ml_image_copy_merge(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 9);
#endif
  return
    ml_image_copy_merge_native(argv[0], argv[1], argv[2], argv[3], 
                                 argv[4], argv[5], argv[6], argv[7],
                                 argv[8]);
}


value ml_image_copy_merge_gray_native(value dst, value src, value dst_x,
                                      value dst_y, value src_x, value src_y,
                                      value w, value h, value pct) {
  gdImageCopyMergeGray(IM_VAL(dst), IM_VAL(src), Int_val(dst_x),
                       Int_val(dst_y), Int_val(src_x), Int_val(src_y),
                       Int_val(w), Int_val(h), Int_val(pct));
  return Val_unit;
}

value ml_image_copy_merge_gray(value *argv, int argc) {
#ifdef SAFER
  assert (argc == 9);
#endif
  return
    ml_image_copy_merge_gray_native(argv[0], argv[1], argv[2], argv[3], 
                                 argv[4], argv[5], argv[6], argv[7],
                                 argv[8]);
}


value ml_image_palette_copy(value dst, value src) {
  gdImagePaletteCopy(IM_VAL(dst), IM_VAL(src));
  return Val_unit;
}
