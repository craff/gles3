#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <fontconfig/fontconfig.h>


static int caml_font_weight(value w)
{
  switch (Int_val(w)) {
  case 0: return FC_WEIGHT_THIN;
  case 1: return FC_WEIGHT_LIGHT;
  case 2: return FC_WEIGHT_REGULAR;
  case 3: return FC_WEIGHT_MEDIUM;
  case 4: return FC_WEIGHT_BOLD;
  case 5: return FC_WEIGHT_BLACK;
  default: return FC_WEIGHT_REGULAR;
  }
}


static int caml_font_slant(value s)
{
  switch (Int_val(s)) {
  case 0: return FC_SLANT_ROMAN;
  case 1: return FC_SLANT_ITALIC;
  case 2: return FC_SLANT_OBLIQUE;
  default: return FC_SLANT_ROMAN;
  }
}


CAMLprim value
caml_fontconfig_find_font(value family,
                          value weight,
                          value slant)
{
  CAMLparam3(family, weight, slant);
  CAMLlocal1(result);

  FcPattern *pat;
  FcPattern *font;
  FcResult res;
  FcChar8 *file = NULL;


  if (!FcInit())
    caml_failwith("FcInit failed");


  pat = FcPatternCreate();
  if (pat == NULL)
    caml_failwith("FcPatternCreate failed");


  FcPatternAddString(
      pat,
      FC_FAMILY,
      (FcChar8 *)String_val(family));


  FcPatternAddInteger(
      pat,
      FC_WEIGHT,
      caml_font_weight(weight));


  FcPatternAddInteger(
      pat,
      FC_SLANT,
      caml_font_slant(slant));


  FcConfigSubstitute(NULL, pat, FcMatchPattern);
  FcDefaultSubstitute(pat);


  font = FcFontMatch(NULL, pat, &res);

  FcPatternDestroy(pat);


  if (font == NULL)
    caml_failwith("no matching font");


  if (FcPatternGetString(font, FC_FILE, 0, &file)
      != FcResultMatch)
  {
    FcPatternDestroy(font);
    caml_failwith("no font file");
  }


  result = caml_copy_string((char *)file);

  FcPatternDestroy(font);

  CAMLreturn(result);
}
