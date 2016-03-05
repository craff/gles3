/****************************************************************************/
/* MLGles3: OpenGL ES3 interface for Objective Caml                         */
/*                                                                          */
/* Copyright (C) 2014   Alexandre Miquel <amiquel@fing.edu.uy>              */
/*                                                                          */
/* MLGles3 is free software: you can redistribute it and/or modify it under */
/* the terms of the  GNU Lesser General Public License  as published by the */
/* Free Software Foundation,  either version 3 of the License,  or (at your */
/* option) any later version.                                               */
/*                                                                          */
/* MLGles3 is distributed  in the hope that it will be useful,  but WITHOUT */
/* ANY WARRANTY;  without even  the implied warranty of MERCHANTABILITY  or */
/* FITNESS  FOR  A PARTICULAR PURPOSE.  See the  GNU  Lesser General Public */
/* License for more details.                                                */
/*                                                                          */
/* You should have received a copy of the GNU Lesser General Public License */
/* along with MLGles2.  If not, see <http://www.gnu.org/licenses/>.         */
/****************************************************************************/
/* ml_gles3.c: ML stubs for Gles3 library                                   */
/****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/bigarray.h>
#include <GLES3/gl3.h>

/*** SIMPLE C -> ML WRAPPERS ***/

#define ML_0(cname)			\
  CAMLprim value ml_##cname (value v)	\
  { cname() ; return Val_unit ; }

#define ML_1(cname, conv1)		    \
  CAMLprim value ml_##cname (value v1)	    \
  { cname(conv1(v1)) ; return Val_unit ; }

#define ML_2(cname, conv1, conv2)		       \
  CAMLprim value ml_##cname (value v1, value v2)       \
  { cname(conv1(v1), conv2(v2)) ; return Val_unit ; }

#define ML_3(cname, conv1, conv2, conv3)			  \
  CAMLprim value ml_##cname (value v1, value v2, value v3)	  \
  { cname(conv1(v1), conv2(v2), conv3(v3)) ; return Val_unit ; }

#define ML_4(cname, conv1, conv2, conv3, conv4)				\
  CAMLprim value ml_##cname (value v1, value v2, value v3, value v4)	\
  { cname(conv1(v1), conv2(v2), conv3(v3), conv4(v4)) ;	return Val_unit ; }

#define ML_5(cname, conv1, conv2, conv3, conv4, conv5)			\
  CAMLprim value ml_##cname (value v1, value v2, value v3, value v4, value v5) \
  { cname(conv1(v1), conv2(v2), conv3(v3), conv4(v4), conv5(v5)) ;	\
    return Val_unit ; }

#define ML_0R(cname, convr)	       \
  CAMLprim value ml_##cname (value v)  \
  { return convr(cname()) ; }

#define ML_1R(cname, conv1, convr)	\
  CAMLprim value ml_##cname (value v1)	\
  { return convr(cname(conv1(v1))) ; }

#define ML_2R(cname, conv1, conv2, convr)	   \
  CAMLprim value ml_##cname (value v1, value v2)   \
  { return convr(cname(conv1(v1), conv2(v2))) ; }

#define ML_3R(cname, conv1, conv2, conv3, convr)	      \
  CAMLprim value ml_##cname (value v1, value v2, value v3)    \
  { return convr(cname(conv1(v1), conv2(v2), conv3(v3))) ; }

#define ML_4R(cname, conv1, conv2, conv3, conv4, convr)			\
  CAMLprim value ml_##cname (value v1, value v2, value v3, value v4)    \
  { return convr(cname(conv1(v1), conv2(v2), conv3(v3), conv4(v4))) ; }

#define GLES_FAIL(msg)   failwith("Gles3." msg)

/*** Big arrays ***/

static long Caml_ba_length_val(value v)
{
  long length = 1 ;
  int i, num_dims = Caml_ba_array_val(v)->num_dims ;
  for(i = 0; i < num_dims; i++)
    length *= Caml_ba_array_val(v)->dim[i] ;
  return length ;
}

static long Caml_ba_elt_bsize_val(value v)
{
  switch(Caml_ba_array_val(v)->flags & BIGARRAY_KIND_MASK) {
  case CAML_BA_FLOAT32: return 4 ;
  case CAML_BA_FLOAT64: return 8 ;
  case CAML_BA_SINT8: return 1 ;
  case CAML_BA_UINT8: return 1 ;
  case CAML_BA_SINT16: return 2 ;
  case CAML_BA_UINT16: return 2 ;
  case CAML_BA_INT32: return 4 ;
  case CAML_BA_INT64: return 8 ;
  case CAML_BA_CAML_INT: return sizeof(value) ;
  case CAML_BA_NATIVE_INT: return sizeof(value) ;
  case CAML_BA_COMPLEX32: return 8 ;
  case CAML_BA_COMPLEX64: return 16 ;
  }
  return 0 ;
}

static long Caml_ba_data_bsize_val(value v)
{
  return Caml_ba_length_val(v) * Caml_ba_elt_bsize_val(v) ;
}

/*** Misc. ***/

#define Float_val(v)       ((GLfloat)Double_val(v))
#define Float_field(v, i)  ((GLfloat)Double_field(v, i))

/****************************************************************************/
/*   CONVERTING ML-TAGS TO/FROM C-ENUMS                                     */
/****************************************************************************/

struct record {
    int key;
    GLenum data;
} ;

static struct record input_table[] = {
#include "gles3_tags.h"
  /* defines TAG_NUMBER, TABLE_SIZE and TABLE_MASK */
} ;

static struct record *tag_table = NULL ;

static void make_tag_table(void)
{
  int i, hash, probe ;
  int sum_probe = 0 ;
  int max_probe = 0 ;
  tag_table = stat_alloc (TABLE_SIZE * sizeof(struct record)) ;
  memset ((char *)tag_table, 0, TABLE_SIZE * sizeof(struct record)) ;
  for (i = 0; i < TAG_NUMBER; i++) {
    hash = (input_table[i].key ^ TABLE_MASK) % TABLE_SIZE ;
    if(hash < 0) hash += TABLE_SIZE ;
    probe = 0 ;
    while(tag_table[hash].key != 0) {
      hash++ ; probe++ ;
      if (hash == TABLE_SIZE) hash = 0 ;
    }
    tag_table[hash].key = input_table[i].key ;
    tag_table[hash].data = input_table[i].data ;
    sum_probe += probe ;
    if(probe > max_probe) max_probe = probe ;
  }
  /*
  fprintf(stderr, "Built hash table: %d/%d entries, mask = 0x%X\n",
	  TAG_NUMBER, TABLE_SIZE, TABLE_MASK) ;
  fprintf(stderr, "Probes: max = %d, sum = %d, avg = %.3f\n",
	  max_probe, sum_probe, (float)sum_probe / TAG_NUMBER) ;
  */
}

GLenum Enum_val(value val)
{
  int tag = Int_val(val) ;
  int hash = (tag ^ TABLE_MASK) % TABLE_SIZE ;
  if(hash < 0) hash += TABLE_SIZE ;
  if(!tag_table) make_tag_table() ;
  while(tag_table[hash].key != tag) {
    if (tag_table[hash].key == 0) failwith("Gles2: Unknown ML tag") ;
    hash++ ;
    if(hash == TABLE_SIZE) hash = 0 ;
  }
  return tag_table[hash].data ;
}

value Val_enum(GLenum enu)
{
  int hash ;
  if(!tag_table) make_tag_table() ;
  for(hash = 0; hash < TABLE_SIZE; hash++)
    if(tag_table[hash].data == enu)
      return Val_int(tag_table[hash].key) ;
  char msg[256];
  sprintf(msg, "Gles: Unknown GL enum: %x",enu);
  failwith(msg) ;
}

/****************************************************************************/
/*   VERTEX ATTRIBUTES & DRAWING                                            */
/****************************************************************************/

ML_2(glVertexAttrib1f, Int_val, Float_val) ;
ML_3(glVertexAttrib2f, Int_val, Float_val, Float_val) ;
ML_4(glVertexAttrib3f, Int_val, Float_val, Float_val, Float_val) ;
ML_5(glVertexAttrib4f, Int_val, Float_val, Float_val, Float_val, Float_val) ;

CAMLprim value ml_glVertexAttribfv(value vi, value vv)
{
  CAMLparam2(vi, vv) ;
  GLint index = Int_val(vi) ;
  int len = Wosize_val(vv) ;
  if(len < 1 || len > 4)
    GLES_FAIL("vertex_attrib_fv: invalid array size") ;
  GLfloat tmp[len] ;
  int k ;
  for(k = 0; k < len; k++)
    tmp[k] = Float_field(vv, k) ;
  switch(len) {
  case 1: glVertexAttrib1fv(index, tmp) ; break ;
  case 2: glVertexAttrib2fv(index, tmp) ; break ;
  case 3: glVertexAttrib3fv(index, tmp) ; break ;
  case 4: glVertexAttrib4fv(index, tmp) ; break ;
  }
  CAMLreturn0 ;
}

ML_1(glEnableVertexAttribArray, Int_val) ;
ML_1(glDisableVertexAttribArray, Int_val) ;

CAMLprim value ml_glVertexAttribBytePointer(value vi, value vs, value vn,
					    value vr, value vd)
{
  CAMLparam5(vi, vs, vn, vr, vd) ;
  GLint index = Int_val(vi) ;
  GLint size = Int_val(vs) ;
  GLboolean norm = Bool_val(vn) ;
  GLsizei stride = Int_val(vr) ;
  void *data = Caml_ba_data_val(vd) ;
  glVertexAttribPointer(index, size, GL_BYTE, norm, stride, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glVertexAttribUBytePointer(value vi, value vs, value vn,
					     value vr, value vd)
{
  CAMLparam5(vi, vs, vn, vr, vd) ;
  GLint index = Int_val(vi) ;
  GLint size = Int_val(vs) ;
  GLboolean norm = Bool_val(vn) ;
  GLsizei stride = Int_val(vr) ;
  void *data = Caml_ba_data_val(vd) ;
  glVertexAttribPointer(index, size, GL_UNSIGNED_BYTE, norm, stride, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glVertexAttribShortPointer(value vi, value vs, value vn,
					     value vr, value vd)
{
  CAMLparam5(vi, vs, vn, vr, vd) ;
  GLint index = Int_val(vi) ;
  GLint size = Int_val(vs) ;
  GLboolean norm = Bool_val(vn) ;
  GLsizei stride = Int_val(vr) ;
  void *data = Caml_ba_data_val(vd) ;
  glVertexAttribPointer(index, size, GL_SHORT, norm, stride, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glVertexAttribUShortPointer(value vi, value vs, value vn,
					      value vr, value vd)
{
  CAMLparam5(vi, vs, vn, vr, vd) ;
  GLint index = Int_val(vi) ;
  GLint size = Int_val(vs) ;
  GLboolean norm = Bool_val(vn) ;
  GLsizei stride = Int_val(vr) ;
  void *data = Caml_ba_data_val(vd) ;
  glVertexAttribPointer(index, size, GL_UNSIGNED_SHORT, norm, stride, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glVertexAttribUIntPointer(value vi, value vs, value vn,
					     value vr, value vd)
{
  CAMLparam5(vi, vs, vn, vr, vd) ;
  GLint index = Int_val(vi) ;
  GLint size = Int_val(vs) ;
  GLboolean norm = Bool_val(vn) ;
  GLsizei stride = Int_val(vr) ;
  void *data = Caml_ba_data_val(vd) ;
  glVertexAttribPointer(index, size, GL_UNSIGNED_INT, norm, stride, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glVertexAttribFloatPointer(value vi, value vs, value vn,
					     value vr, value vd)
{
  CAMLparam5(vi, vs, vn, vr, vd) ;
  GLint index = Int_val(vi) ;
  GLint size = Int_val(vs) ;
  GLboolean norm = Bool_val(vn) ;
  GLsizei stride = Int_val(vr) ;
  void *data = Caml_ba_data_val(vd) ;
  glVertexAttribPointer(index, size, GL_FLOAT, norm, stride, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glVertexAttribBufferPointer(value vi, value vs, value vt,
					      value vn, value vr, value vd)
{
  CAMLparam5(vi, vs, vt, vn, vr) ;
  CAMLxparam1(vd) ;
  GLint index = Int_val(vi) ;
  GLint size = Int_val(vs) ;
  GLenum type = Enum_val(vt) ;
  GLboolean norm = Bool_val(vn) ;
  GLsizei stride = Int_val(vr) ;
  void *data = (void *)Long_val(vd) ;
  glVertexAttribPointer(index, size, type, norm, stride, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glVertexAttribBufferPointer_bc(value *argv, int argn)
{
  return ml_glVertexAttribBufferPointer(argv[0], argv[1], argv[2],
					argv[3], argv[4], argv[5]) ;
}

ML_3(glDrawArrays, Enum_val, Int_val, Int_val) ;

CAMLprim value ml_glDrawUByteElements(value vm, value vc, value vd)
{
  CAMLparam3(vm, vc, vd) ;
  GLenum mode = Enum_val(vm) ;
  GLsizei count = Int_val(vc) ;
  void *data = Caml_ba_data_val(vd) ;
  glDrawElements(mode, count, GL_UNSIGNED_BYTE, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glDrawUShortElements(value vm, value vc, value vd)
{
  CAMLparam3(vm, vc, vd) ;
  GLenum mode = Enum_val(vm) ;
  GLsizei count = Int_val(vc) ;
  void *data = Caml_ba_data_val(vd) ;
  glDrawElements(mode, count, GL_UNSIGNED_SHORT, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glDrawUIntElements(value vm, value vc, value vd)
{
  CAMLparam3(vm, vc, vd) ;
  GLenum mode = Enum_val(vm) ;
  GLsizei count = Int_val(vc) ;
  void *data = Caml_ba_data_val(vd) ;
  glDrawElements(mode, count, GL_UNSIGNED_INT, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glDrawBufferElements(value vm, value vc, value vt, value vd)
{
  CAMLparam4(vm, vc, vt, vd) ;
  GLenum mode = Enum_val(vm) ;
  GLsizei count = Int_val(vc) ;
  GLenum type = Enum_val(vt) ;
  void *data = (void *)Long_val(vd) ;
  glDrawElements(mode, count, type, data) ;
  CAMLreturn0 ;
}

/****************************************************************************/
/*   BUFFERS                                                                */
/****************************************************************************/

ML_1R(glIsBuffer, Int_val, Val_bool) ;

CAMLprim value ml_glGenBuffer(value v)
{
  CAMLparam1(v) ;
  GLuint buf ;
  glGenBuffers(1, &buf) ;
  CAMLreturn(Val_int(buf)) ;
}

CAMLprim value ml_glGenBuffers(value vn)
{
  CAMLparam1(vn) ;
  CAMLlocal1(ret) ;
  GLint i, n = Int_val(vn) ;
  GLuint buf[n] ;
  glGenBuffers(n, buf) ;
  ret = caml_alloc_tuple(n) ;
  for(i = 0; i < n; i++)
    Store_field(ret, i, Val_int(buf[i])) ;
  CAMLreturn(ret) ;
}

CAMLprim value ml_glDeleteBuffer(value vb)
{
  CAMLparam1(vb) ;
  GLuint buf = Int_val(vb) ;
  glDeleteBuffers(1, &buf) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glDeleteBuffers(value vv)
{
  CAMLparam1(vv) ;
  GLint i, n = Wosize_val(vv) ;
  GLuint buf[n] ;
  for(i = 0; i < n; i++)
    buf[i] = Int_val(Field(vv, i)) ;
  glDeleteBuffers(n, buf) ;
  CAMLreturn0 ;
}

ML_2(glBindBuffer, Enum_val, Int_val) ;

CAMLprim value ml_glBufferSize(value vt, value vs, value vu)
{
  CAMLparam3(vt, vs, vu) ;
  GLenum target = Enum_val(vt) ;
  GLint size = Int_val(vs) ;
  GLenum usage = Enum_val(vu) ;
  glBufferData(target, size, NULL, usage) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glBufferData(value vt, value vd, value vu)
{
  CAMLparam3(vt, vd, vu) ;
  GLenum target = Enum_val(vt) ;
  int size = Caml_ba_data_bsize_val(vd) ;
  void *data = Caml_ba_data_val(vd) ;
  GLenum usage = Enum_val(vu) ;
  glBufferData(target, size, data, usage) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glBufferSubData(value vt, value vo, value vd)
{
  CAMLparam3(vt, vo, vd) ;
  GLenum target = Enum_val(vt) ;
  int offset = Int_val(vo) ;
  int size = Caml_ba_data_bsize_val(vd) ;
  void *data = Caml_ba_data_val(vd) ;
  glBufferSubData(target, offset, size, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glGetBufferSize(value vt)
{
  CAMLparam1(vt) ;
  GLenum target = Enum_val(vt) ;
  GLint param ;
  glGetBufferParameteriv(target, GL_BUFFER_SIZE, &param) ;
  CAMLreturn(Val_int(param)) ;
}

CAMLprim value ml_glGetBufferUsage(value vt)
{
  CAMLparam1(vt) ;
  GLenum target = Enum_val(vt) ;
  GLint param ;
  glGetBufferParameteriv(target, GL_BUFFER_USAGE, &param) ;
  CAMLreturn(Val_int(param)) ;
}

/****************************************************************************/
/*   SHADERS                                                                */
/****************************************************************************/

ML_1R(glIsShader, Int_val, Val_bool) ;
ML_1R(glCreateShader, Enum_val, Val_int) ;
ML_1(glDeleteShader, Int_val) ;

CAMLprim value ml_glShaderSource(value vs, value vv)
{
  CAMLparam2(vs, vv) ;
  GLuint shader = Int_val(vs) ;
  GLsizei i, count = Wosize_val(vv) ;
  const GLchar *tmp[count] ;
  for(i = 0; i < count; i++)
    tmp[i] = String_val(Field(vv, i)) ;
  glShaderSource(shader, count, tmp, NULL) ;
  CAMLreturn0 ;
}

ML_1(glCompileShader, Int_val) ;
ML_0(glReleaseShaderCompiler) ;

CAMLprim value ml_glGetShaderType(value vs)
{
  CAMLparam1(vs) ;
  GLuint shad = Int_val(vs) ;
  GLint type ;
  glGetShaderiv(shad, GL_SHADER_TYPE, &type) ;
  CAMLreturn(Val_enum(type)) ;
}

CAMLprim value ml_glGetShaderDeleteStatus(value vs)
{
  CAMLparam1(vs) ;
  GLuint shad = Int_val(vs) ;
  GLint status ;
  glGetShaderiv(shad, GL_DELETE_STATUS, &status) ;
  CAMLreturn(Val_bool(status)) ;
}

CAMLprim value ml_glGetShaderCompileStatus(value vs)
{
  CAMLparam1(vs) ;
  GLuint shad = Int_val(vs) ;
  GLint status ;
  glGetShaderiv(shad, GL_COMPILE_STATUS, &status) ;
  CAMLreturn(Val_bool(status)) ;
}

CAMLprim value ml_glGetShaderInfoLog(value vs)
{
  CAMLparam1(vs) ;
  GLuint shad = Int_val(vs) ;
  GLint len ;
  glGetShaderiv(shad, GL_INFO_LOG_LENGTH, &len) ;
  int bufsiz = len + 10 ;
  char buffer[bufsiz] ;
  glGetShaderInfoLog(shad, bufsiz, NULL, buffer) ;
  CAMLreturn(caml_copy_string(buffer)) ;
}

CAMLprim value ml_glGetShaderSource(value vs)
{
  CAMLparam1(vs) ;
  GLuint shad = Int_val(vs) ;
  GLint len ;
  glGetShaderiv(shad, GL_SHADER_SOURCE_LENGTH, &len) ;
  int bufsiz = len + 1 ;
  char buffer[bufsiz] ;
  glGetShaderSource(shad, bufsiz, NULL, buffer) ;
  CAMLreturn(caml_copy_string(buffer)) ;
}

/****************************************************************************/
/*   PROGRAMS                                                               */
/****************************************************************************/

ML_1R(glIsProgram, Int_val, Val_bool) ;
ML_0R(glCreateProgram, Val_int) ;
ML_1(glDeleteProgram, Int_val) ;

ML_2(glAttachShader, Int_val, Int_val) ;
ML_2(glDetachShader, Int_val, Int_val) ;

ML_1(glLinkProgram, Int_val) ;
ML_1(glUseProgram, Int_val) ;

CAMLprim value ml_glValidateProgram(value vp)
{
  CAMLparam1(vp) ;
  GLuint prog = Int_val(vp) ;
  glValidateProgram(prog) ;
  GLint status ;
  glGetProgramiv(prog, GL_VALIDATE_STATUS, &status) ;
  CAMLreturn(Val_bool(status)) ;
}

CAMLprim value ml_glGetActiveAttribs(value vp)
{
  CAMLparam1(vp) ;
  CAMLlocal3(list, cons, triple) ;
  GLuint prog = Int_val(vp) ;
  const int bufsiz = 256 ;
  char buffer[bufsiz] ;
  GLint i, num, size ;
  GLenum type ;
  list = Val_unit ;
  glGetProgramiv(prog, GL_ACTIVE_ATTRIBUTES, &num) ;
  for(i = num - 1; i >= 0; i--) {
    glGetActiveAttrib(prog, i, bufsiz, NULL, &size, &type, buffer) ;
    triple = caml_alloc_tuple(3) ;
    Store_field(triple, 0, caml_copy_string(buffer)) ;
    Store_field(triple, 1, Val_enum(type)) ;
    Store_field(triple, 2, Val_int(size)) ;
    cons = caml_alloc_tuple(2) ;
    Store_field(cons, 0, triple) ;
    Store_field(cons, 1, list) ;
    list = cons ;
  }
  CAMLreturn(list) ;
}

ML_2R(glGetAttribLocation, Int_val, String_val, Val_int) ;
ML_3(glBindAttribLocation, Int_val, Int_val, String_val) ;

CAMLprim value ml_glGetActiveUniforms(value vp)
{
  CAMLparam1(vp) ;
  CAMLlocal3(list, cons, triple) ;
  GLuint prog = Int_val(vp) ;
  const int bufsiz = 256 ;
  char buffer[bufsiz] ;
  GLint i, num, size ;
  GLenum type ;
  list = Val_unit ;
  glGetProgramiv(prog, GL_ACTIVE_UNIFORMS, &num) ;
  for(i = num - 1; i >= 0; i--) {
    glGetActiveUniform(prog, i, bufsiz, NULL, &size, &type, buffer) ;
    triple = caml_alloc_tuple(3) ;
    Store_field(triple, 0, caml_copy_string(buffer)) ;
    Store_field(triple, 1, Val_enum(type)) ;
    Store_field(triple, 2, Val_int(size)) ;
    cons = caml_alloc_tuple(2) ;
    Store_field(cons, 0, triple) ;
    Store_field(cons, 1, list) ;
    list = cons ;
  }
  CAMLreturn(list) ;
}

ML_2R(glGetUniformLocation, Int_val, String_val, Val_int) ;

CAMLprim value ml_glGetAttachedShaders(value vp)
{
  CAMLparam1(vp) ;
  CAMLlocal1(ret) ;
  GLuint prog = Int_val(vp) ;
  GLint i, n ;
  glGetProgramiv(prog, GL_ATTACHED_SHADERS, &n) ;
  GLuint tmp[n] ;
  glGetAttachedShaders(prog, n, NULL, tmp) ;
  ret = caml_alloc_tuple(n) ;
  for(i = 0; i < n; i++)
    Store_field(ret, i, Val_int(tmp[i])) ;
  CAMLreturn(ret) ;
}

CAMLprim value ml_glGetProgramInfoLog(value vp)
{
  CAMLparam1(vp) ;
  GLuint prog = Int_val(vp) ;
  GLint len ;
  glGetProgramiv(prog, GL_INFO_LOG_LENGTH, &len) ;
  int bufsiz = len + 1 ;
  char buffer[bufsiz] ;
  glGetProgramInfoLog(prog, bufsiz, NULL, buffer) ;
  CAMLreturn(caml_copy_string(buffer)) ;
}

CAMLprim value ml_glGetProgramDeleteStatus(value vp)
{
  CAMLparam1(vp) ;
  GLuint prog = Int_val(vp) ;
  GLint status ;
  glGetProgramiv(prog, GL_DELETE_STATUS, &status) ;
  CAMLreturn(Val_bool(status)) ;
}

CAMLprim value ml_glGetProgramLinkStatus(value vp)
{
  CAMLparam1(vp) ;
  GLuint prog = Int_val(vp) ;
  GLint status ;
  glGetProgramiv(prog, GL_LINK_STATUS, &status) ;
  CAMLreturn(Val_bool(status)) ;
}

/****************************************************************************/
/*   UNIFORMS                                                               */
/****************************************************************************/

ML_2(glUniform1i, Int_val, Int_val) ;
ML_3(glUniform2i, Int_val, Int_val, Int_val) ;
ML_4(glUniform3i, Int_val, Int_val, Int_val, Int_val) ;
ML_5(glUniform4i, Int_val, Int_val, Int_val, Int_val, Int_val) ;

CAMLprim value ml_glUniform1iv(value vl, value vc, value vv)
{
  CAMLparam3(vl, vc, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count ;
  if(len < 0 || len > Wosize_val(vv))
    GLES_FAIL("uniform_1iv: count out of bounds") ;
  GLint tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = Int_val(Field(vv, i)) ;
  glUniform1iv(loc, count, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniform2iv(value vl, value vc, value vv)
{
  CAMLparam3(vl, vc, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 2 ;
  if(len < 0 || len > Wosize_val(vv))
    GLES_FAIL("uniform_2iv: count out of bounds") ;
  GLint tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = Int_val(Field(vv, i)) ;
  glUniform2iv(loc, count, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniform3iv(value vl, value vc, value vv)
{
  CAMLparam3(vl, vc, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 3 ;
  if(len < 0 || len > Wosize_val(vv))
    GLES_FAIL("uniform_3iv: count out of bounds") ;
  GLint tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = Int_val(Field(vv, i)) ;
  glUniform3iv(loc, count, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniform4iv(value vl, value vc, value vv)
{
  CAMLparam3(vl, vc, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 4 ;
  if(len < 0 || len > Wosize_val(vv))
    GLES_FAIL("uniform_4iv: count out of bounds") ;
  GLint tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = Int_val(Field(vv, i)) ;
  glUniform4iv(loc, count, tmp) ;
  CAMLreturn0 ;
}

ML_2(glUniform1f, Int_val, Float_val) ;
ML_3(glUniform2f, Int_val, Float_val, Float_val) ;
ML_4(glUniform3f, Int_val, Float_val, Float_val, Float_val) ;
ML_5(glUniform4f, Int_val, Float_val, Float_val, Float_val, Float_val) ;

CAMLprim value ml_glUniform1fv(value vl, value vc, value vv)
{
  CAMLparam3(vl, vc, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count ;
  if(len < 0 || len > (Wosize_val(vv) / Double_wosize))
    GLES_FAIL("uniform_1fv: count out of bounds") ;
  GLfloat tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = (GLfloat)Double_field(vv, i) ;
  glUniform1fv(loc, count, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniform2fv(value vl, value vc, value vv)
{
  CAMLparam3(vl, vc, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 2 ;
  if(len < 0 || len > (Wosize_val(vv) / Double_wosize))
    GLES_FAIL("uniform_2fv: count out of bounds") ;
  GLfloat tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = (GLfloat)Double_field(vv, i) ;
  glUniform2fv(loc, count, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniform3fv(value vl, value vc, value vv)
{
  CAMLparam3(vl, vc, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 3 ;
  if(len < 0 || len > (Wosize_val(vv) / Double_wosize))
    GLES_FAIL("uniform_3fv: count out of bounds") ;
  GLfloat tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = (GLfloat)Double_field(vv, i) ;
  glUniform3fv(loc, count, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniform4fv(value vl, value vc, value vv)
{
  CAMLparam3(vl, vc, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 4 ;
  if(len < 0 || len > (Wosize_val(vv) / Double_wosize))
    GLES_FAIL("uniform_4fv: count out of bounds") ;
  GLfloat tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = (GLfloat)Double_field(vv, i) ;
  glUniform4fv(loc, count, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniformMatrix2fv(value vl, value vc, value vt, value vv)
{
  CAMLparam4(vl, vc, vt, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 4 ;
  if(len < 0 || len > (Wosize_val(vv) / Double_wosize))
    GLES_FAIL("uniform_matrix_2fv: count out of bounds") ;
  GLfloat tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = (GLfloat)Double_field(vv, i) ;
  GLboolean transp = Bool_val(vt) ;
  glUniformMatrix2fv(loc, count, transp, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniformMatrix3fv(value vl, value vc, value vt, value vv)
{
  CAMLparam4(vl, vc, vt, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 9 ;
  if(len < 0 || len > (Wosize_val(vv) / Double_wosize))
    GLES_FAIL("uniform_matrix_3fv: count out of bounds") ;
  GLfloat tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = (GLfloat)Double_field(vv, i) ;
  GLboolean transp = Bool_val(vt) ;
  glUniformMatrix3fv(loc, count, transp, tmp) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glUniformMatrix4fv(value vl, value vc, value vt, value vv)
{
  CAMLparam4(vl, vc, vt, vv) ;
  GLint loc = Int_val(vl) ;
  GLsizei count = Int_val(vc) ;
  GLsizei i, len = count * 16 ;
  if(len < 0 || len > (Wosize_val(vv) / Double_wosize))
    GLES_FAIL("uniform_matrix_4fv: count out of bounds") ;
  GLfloat tmp[len] ;
  for(i = 0; i < len; i++)
    tmp[i] = (GLfloat)Double_field(vv, i) ;
  GLboolean transp = Bool_val(vt) ;
  glUniformMatrix4fv(loc, count, transp, tmp) ;
  CAMLreturn0 ;
}

/****************************************************************************/
/*   RASTERIZATION                                                          */
/****************************************************************************/

ML_2(glDepthRangef, Float_val, Float_val) ;
ML_4(glViewport, Int_val, Int_val, Int_val, Int_val) ;

ML_1R(glIsEnabled, Enum_val, Val_bool) ;
ML_1(glEnable, Enum_val) ;
ML_1(glDisable, Enum_val) ;

ML_1(glLineWidth, Float_val) ;
ML_1(glFrontFace, Enum_val) ;
ML_1(glCullFace, Enum_val) ;
ML_2(glPolygonOffset, Float_val, Float_val) ;

/****************************************************************************/
/*   TEXTURES                                                               */
/****************************************************************************/

ML_1R(glIsTexture, Int_val, Val_bool) ;

CAMLprim value ml_glGenTexture(value v)
{
  CAMLparam1(v) ;
  GLuint tex ;
  glGenTextures(1, &tex) ;
  CAMLreturn(Val_int(tex)) ;
}

CAMLprim value ml_glGenTextures(value vn)
{
  CAMLparam1(vn) ;
  CAMLlocal1(ret) ;
  GLint i, n = Int_val(vn) ;
  GLuint tex[n] ;
  glGenTextures(n, tex) ;
  ret = caml_alloc_tuple(n) ;
  for(i = 0; i < n; i++)
    Store_field(ret, i, Val_int(tex[i])) ;
  CAMLreturn(ret) ;
}

CAMLprim value ml_glDeleteTexture(value vb)
{
  CAMLparam1(vb) ;
  GLuint tex = Int_val(vb) ;
  glDeleteTextures(1, &tex) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glDeleteTextures(value vv)
{
  CAMLparam1(vv) ;
  GLint i, n = Wosize_val(vv) ;
  GLuint tex[n] ;
  for(i = 0; i < n; i++)
    tex[i] = Int_val(Field(vv, i)) ;
  glDeleteTextures(n, tex) ;
  CAMLreturn0 ;
}

ML_2(glBindTexture, Enum_val, Int_val) ;

CAMLprim value ml_glActiveTexture(value vi)
{
  CAMLparam1(vi) ;
  int i = Int_val(vi) ;
  glActiveTexture(GL_TEXTURE0 + i) ;
  CAMLreturn0 ;
}

static int pixel_bsize(GLenum format)
{
  switch(format) {
  case GL_ALPHA: return 1 ;
  case GL_RGB: return 3 ;
  case GL_RGBA: return 4 ;
  case GL_LUMINANCE: return 1 ;
  case GL_LUMINANCE_ALPHA: return 2;
  }
  return 0 ;
}

static GLenum format_from_internal(GLenum format)
{
  switch(format) {
  case GL_ALPHA: return GL_ALPHA ;
  case GL_RGB: return GL_RGB ;
  case GL_RGBA: return GL_RGBA ;
  case GL_LUMINANCE: return GL_LUMINANCE ;
  case GL_LUMINANCE_ALPHA: return GL_LUMINANCE_ALPHA;
  case GL_DEPTH_COMPONENT16:
  case GL_DEPTH_COMPONENT24:
    return GL_DEPTH_COMPONENT;
  case GL_DEPTH24_STENCIL8:
    return GL_DEPTH_STENCIL;
  }
  return 0 ;
}

static GLenum type_from_internal(GLenum format)
{
  switch(format) {
  case GL_ALPHA: return GL_UNSIGNED_BYTE ;
  case GL_RGB: return GL_UNSIGNED_BYTE ;
  case GL_RGBA: return GL_UNSIGNED_BYTE ;
  case GL_LUMINANCE: return GL_UNSIGNED_BYTE ;
  case GL_LUMINANCE_ALPHA: return GL_UNSIGNED_BYTE;
  case GL_DEPTH_COMPONENT16: return GL_UNSIGNED_SHORT;
  case GL_DEPTH_COMPONENT24:
  case GL_DEPTH24_STENCIL8:
    return GL_UNSIGNED_INT;
  }
  return 0 ;
}

CAMLprim value ml_glTexImage2D(value vt, value vl, value vimg)
{
  CAMLparam3(vt, vl, vimg) ;
  CAMLlocal1(ba) ;
  GLenum target = Enum_val(vt) ;
  GLint level = Int_val(vl) ;
  GLint width = Int_val(Field(vimg, 0)) ;
  GLint height = Int_val(Field(vimg, 1)) ;
  GLenum format = Enum_val(Field(vimg, 2)) ;
  ba = Field(vimg, 3) ;
  if(Caml_ba_data_bsize_val(ba) < width * height * pixel_bsize(format))
    GLES_FAIL("tex_image_2d: too few data bytes") ;
  void *data = Caml_ba_data_val(ba) ;
  glTexImage2D(target, level, format, width, height, 0,
	       format, GL_UNSIGNED_BYTE, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glTexNullImage2D(value vt, value vl, value vw, value vh, value vif)
{
  CAMLparam5(vt, vl, vw, vh, vif) ;
  GLenum target = Enum_val(vt) ;
  GLint level = Int_val(vl) ;
  GLint width = Int_val(vw) ;
  GLint height = Int_val(vh) ;
  GLenum internal_format = Enum_val(vif) ;
  GLenum format = format_from_internal(internal_format);
  GLenum type = type_from_internal(internal_format);
  glTexImage2D(target, level, internal_format, width, height, 0,
	       format, type, 0) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glTexSubImage2D(value vt, value vl,
				  value vx, value vy, value vimg)
{
  CAMLparam5(vt, vl, vx, vy, vimg) ;
  CAMLlocal1(ba) ;
  GLenum target = Enum_val(vt) ;
  GLint level = Int_val(vl) ;
  GLint xoffset = Int_val(vx) ;
  GLint yoffset = Int_val(vy) ;
  GLint width = Int_val(Field(vimg, 0)) ;
  GLint height = Int_val(Field(vimg, 1)) ;
  GLenum format = Enum_val(Field(vimg, 2)) ;
  ba = Field(vimg, 3) ;
  if(Caml_ba_data_bsize_val(ba) < width * height * pixel_bsize(format))
    GLES_FAIL("tex_image_2d: too few data bytes") ;
  void *data = Caml_ba_data_val(ba) ;
  glTexSubImage2D(target, level, xoffset, yoffset, width, height,
	       format, GL_UNSIGNED_BYTE, data) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glCopyTexImage2D(value vt, value vl, value vf, value vr)
{
  CAMLparam4(vt, vl, vf, vr) ;
  CAMLlocal1(ba) ;
  GLenum target = Enum_val(vt) ;
  GLint level = Int_val(vl) ;
  GLenum format = Enum_val(vf) ;
  GLint x = Int_val(Field(vr, 0)) ;
  GLint y = Int_val(Field(vr, 1)) ;
  GLint width = Int_val(Field(vr, 2)) ;
  GLint height = Int_val(Field(vr, 3)) ;
  glCopyTexImage2D(target, level, format, x, y, width, height, 0) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glCopyTexSubImage2D(value vt, value vl,
				      value vx, value vy, value vr)
{
  CAMLparam5(vt, vl, vx, vy, vr) ;
  CAMLlocal1(ba) ;
  GLenum target = Enum_val(vt) ;
  GLint level = Int_val(vl) ;
  GLint xoffset = Int_val(vx) ;
  GLint yoffset = Int_val(vy) ;
  GLint x = Int_val(Field(vr, 0)) ;
  GLint y = Int_val(Field(vr, 1)) ;
  GLint width = Int_val(Field(vr, 2)) ;
  GLint height = Int_val(Field(vr, 3)) ;
  glCopyTexSubImage2D(target, level, xoffset, yoffset, x, y, width, height) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glTexParameter(value vt, value vp)
{
  CAMLparam2(vt, vp) ;
  GLenum target = Enum_val(vt) ;
  GLenum param = Enum_val(Field(vp, 0)) ;
  GLenum val = Enum_val(Field(vp, 1)) ;
  glTexParameteri(target, param, val) ;
  CAMLreturn0 ;
}

ML_1(glGenerateMipmap, Enum_val) ;

/****************************************************************************/
/*   PER-FRAGMENT OPERATIONS                                                */
/****************************************************************************/

ML_4(glScissor, Int_val, Int_val, Int_val, Int_val) ;

ML_2(glSampleCoverage, Float_val, Bool_val) ;

ML_3(glStencilFunc, Enum_val, Int_val, Int_val) ;
ML_4(glStencilFuncSeparate, Enum_val, Enum_val, Int_val, Int_val) ;
ML_3(glStencilOp, Enum_val, Enum_val, Enum_val) ;
ML_4(glStencilOpSeparate, Enum_val, Enum_val, Enum_val, Enum_val) ;

ML_1(glDepthFunc, Enum_val) ;

ML_1(glBlendEquation, Enum_val) ;
ML_2(glBlendEquationSeparate, Enum_val, Enum_val) ;

ML_2(glBlendFunc, Enum_val, Enum_val) ;
ML_4(glBlendFuncSeparate, Enum_val, Enum_val, Enum_val, Enum_val) ;

CAMLprim value ml_glBlendColor(value vc)
{
  CAMLparam1(vc) ;
  GLfloat red = (GLfloat)Double_field(vc, 0) ;
  GLfloat green = (GLfloat)Double_field(vc, 1) ;
  GLfloat blue = (GLfloat)Double_field(vc, 2) ;
  GLfloat alpha = (GLfloat)Double_field(vc, 3) ;
  glBlendColor(red, green, blue, alpha) ;
  CAMLreturn0 ;
}

/****************************************************************************/
/*   WHOLE FRAMEBUFFER OPERATIONS                                           */
/****************************************************************************/

ML_4(glColorMask, Bool_val, Bool_val, Bool_val, Bool_val) ;
ML_1(glDepthMask, Bool_val) ;
ML_1(glStencilMask, Int_val) ;
ML_2(glStencilMaskSeparate, Enum_val, Int_val) ;

CAMLprim value ml_glClear(value vl)
{
  CAMLparam1(vl) ;
  GLint accu = 0 ;
  while(Is_block(vl)) {
    accu |= Enum_val(Field(vl, 0)) ;
    vl = Field(vl, 1) ;
  }
  glClear(accu) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glClearColor(value vc)
{
  CAMLparam1(vc) ;
  GLfloat red = (GLfloat)Double_field(vc, 0) ;
  GLfloat green = (GLfloat)Double_field(vc, 1) ;
  GLfloat blue = (GLfloat)Double_field(vc, 2) ;
  GLfloat alpha = (GLfloat)Double_field(vc, 3) ;
  glClearColor(red, green, blue, alpha) ;
  CAMLreturn0 ;
}

ML_1(glClearDepthf, Float_val) ;
ML_1(glClearStencil, Int_val) ;

CAMLprim value ml_glReadPixels(value vx, value vy, value vimg)
{
  CAMLparam3(vx, vy, vimg) ;
  CAMLlocal1(ba) ;
  GLint x = Int_val(vx) ;
  GLint y = Int_val(vy) ;
  GLint width = Int_val(Field(vimg, 0)) ;
  GLint height = Int_val(Field(vimg, 1)) ;
  GLenum format = Enum_val(Field(vimg, 2)) ;
  ba = Field(vimg, 3) ;
  if(Caml_ba_data_bsize_val(ba) < width * height * pixel_bsize(format))
    GLES_FAIL("read_pixels: too few data bytes") ;
  void *data = Caml_ba_data_val(ba) ;
  glReadPixels(x, y, width, height, format, GL_UNSIGNED_BYTE, data) ;
  CAMLreturn0 ;
}

/****************************************************************************/
/*   RENDERBUFFERS                                                          */
/****************************************************************************/

ML_1R(glIsRenderbuffer, Int_val, Val_bool) ;

CAMLprim value ml_glGenRenderbuffer(value v)
{
  CAMLparam1(v) ;
  GLuint buf ;
  glGenRenderbuffers(1, &buf) ;
  CAMLreturn(Val_int(buf)) ;
}

CAMLprim value ml_glGenRenderbuffers(value vn)
{
  CAMLparam1(vn) ;
  CAMLlocal1(ret) ;
  GLint i, n = Int_val(vn) ;
  GLuint buf[n] ;
  glGenRenderbuffers(n, buf) ;
  ret = caml_alloc_tuple(n) ;
  for(i = 0; i < n; i++)
    Store_field(ret, i, Val_int(buf[i])) ;
  CAMLreturn(ret) ;
}

CAMLprim value ml_glDeleteRenderbuffer(value vb)
{
  CAMLparam1(vb) ;
  GLuint buf = Int_val(vb) ;
  glDeleteRenderbuffers(1, &buf) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glDeleteRenderbuffers(value vv)
{
  CAMLparam1(vv) ;
  GLint i, n = Wosize_val(vv) ;
  GLuint buf[n] ;
  for(i = 0; i < n; i++)
    buf[i] = Int_val(Field(vv, i)) ;
  glDeleteRenderbuffers(n, buf) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glDrawBuffers(value vv)
{
  CAMLparam1(vv) ;
  GLint i, n = Wosize_val(vv) ;
  GLenum buf[n] ;
  for(i = 0; i < n; i++)
    buf[i] = Enum_val(Field(vv, i)) ;
  glDrawBuffers(n, buf) ;
  CAMLreturn0 ;
}

ML_2(glBindRenderbuffer, Enum_val, Int_val) ;
ML_4(glRenderbufferStorage, Enum_val, Enum_val, Int_val, Int_val) ;
ML_1R(glCheckFramebufferStatus, Enum_val, Val_enum) ;

/****************************************************************************/
/*   FRAMEBUFFERS                                                           */
/****************************************************************************/

ML_1R(glIsFramebuffer, Int_val, Val_bool) ;

CAMLprim value ml_glGenFramebuffer(value v)
{
  CAMLparam1(v) ;
  GLuint buf ;
  glGenFramebuffers(1, &buf) ;
  CAMLreturn(Val_int(buf)) ;
}

CAMLprim value ml_glGenFramebuffers(value vn)
{
  CAMLparam1(vn) ;
  CAMLlocal1(ret) ;
  GLint i, n = Int_val(vn) ;
  GLuint buf[n] ;
  glGenFramebuffers(n, buf) ;
  ret = caml_alloc_tuple(n) ;
  for(i = 0; i < n; i++)
    Store_field(ret, i, Val_int(buf[i])) ;
  CAMLreturn(ret) ;
}

CAMLprim value ml_glDeleteFramebuffer(value vb)
{
  CAMLparam1(vb) ;
  GLuint buf = Int_val(vb) ;
  glDeleteFramebuffers(1, &buf) ;
  CAMLreturn0 ;
}

CAMLprim value ml_glDeleteFramebuffers(value vv)
{
  CAMLparam1(vv) ;
  GLint i, n = Wosize_val(vv) ;
  GLuint buf[n] ;
  for(i = 0; i < n; i++)
    buf[i] = Int_val(Field(vv, i)) ;
  glDeleteFramebuffers(n, buf) ;
  CAMLreturn0 ;
}

ML_2(glBindFramebuffer, Enum_val, Int_val) ;
ML_4(glFramebufferRenderbuffer, Enum_val, Enum_val, Enum_val, Int_val) ;
ML_5(glFramebufferTexture2D, Enum_val, Enum_val, Enum_val, Int_val, Int_val) ;

/****************************************************************************/
/*   MISCELLANEOUS                                                          */
/****************************************************************************/

ML_0R(glGetError, Val_enum) ;

CAMLprim value ml_glGetVendor(value v)
{
  CAMLparam1(v) ;
  char *s = (char *)glGetString(GL_VENDOR) ;
  CAMLreturn(caml_copy_string(s)) ;
}

CAMLprim value ml_glGetRenderer(value v)
{
  CAMLparam1(v) ;
  char *s = (char *)glGetString(GL_RENDERER) ;
  CAMLreturn(caml_copy_string(s)) ;
}

CAMLprim value ml_glGetVersion(value v)
{
  CAMLparam1(v) ;
  char *s = (char *)glGetString(GL_VERSION) ;
  CAMLreturn(caml_copy_string(s)) ;
}

CAMLprim value ml_glGetShadingLanguageVersion(value v)
{
  CAMLparam1(v) ;
  char *s = (char *)glGetString(GL_SHADING_LANGUAGE_VERSION) ;
  CAMLreturn(caml_copy_string(s)) ;
}

CAMLprim value ml_glGetExtensions(value v)
{
  CAMLparam1(v) ;
  char *s = (char *)glGetString(GL_EXTENSIONS) ;
  CAMLreturn(caml_copy_string(s)) ;
}

ML_0(glFlush) ;
ML_0(glFinish) ;
