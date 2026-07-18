#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/fail.h>
#include <caml/custom.h>

#include <ft2build.h>
#include FT_FREETYPE_H

static FT_Library library;

CAMLprim value ml_ft_init(value unit)
{
    if (FT_Init_FreeType(&library))
        caml_failwith("FT_Init_FreeType");
    return Val_unit;
}

CAMLprim value ml_ft_load_glyph(value filename,
                                value size,
                                value codepoint)
{
    CAMLparam3(filename,size,codepoint);
    CAMLlocal1(result);

    FT_Face face;

    if (FT_New_Face(library, String_val(filename), 0, &face))
        caml_failwith("FT_New_Face");
    FT_Set_Pixel_Sizes(face,0,Int_val(size));

    if (FT_Load_Char(face,
                     Int_val(codepoint),
                     FT_LOAD_RENDER))
        caml_failwith("FT_Load_Char");

    FT_Bitmap *bmp = &face->glyph->bitmap;

    int rows = bmp->rows;
    int pitch = bmp->pitch;

    uint8_t *src = bmp->buffer;

    uint8_t *copy = caml_stat_alloc(rows * pitch);

    for (int y = 0; y < rows; y++) {
      memcpy(copy + (rows - 1 - y) * pitch,
	     src + y * pitch,
	     pitch);
    }
    intnat dims[1];
    dims[0] = rows * pitch;

    value ba =
      caml_ba_alloc(CAML_BA_UINT8 | CAML_BA_C_LAYOUT,
		    1,
		    copy,
		    dims);

    result = caml_alloc_tuple(7);

    Store_field(result,0,ba);
    Store_field(result,1,Val_int(bmp->width));
    Store_field(result,2,Val_int(bmp->rows));
    Store_field(result,3,Val_int(bmp->pitch));
    Store_field(result,4,Val_int(face->glyph->bitmap_left));
    Store_field(result,5,Val_int(face->glyph->bitmap_top));
    Store_field(result,6,Val_int(face->glyph->advance.x>>6));

    FT_Done_Face(face);
    CAMLreturn(result);
}
