/***************************************************************************/
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
/* along with MLGles3.  If not, see <http://www.gnu.org/licenses/>.         */
/****************************************************************************/
/* ml_gles3x.c: ML stubs for Gles3x companion library                       */
/****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <EGL/egl.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <caml/version.h>
#include <caml/custom.h>
#include <sys/time.h>
#include "ml_egl.h"
#include "ml_egl_platform.h"

#define MODIFIER(x,code) egl_mod EGL_MOD_##x = code;
#include "modifier.h"
#undef MODIFIER

#if OCAML_VERSION_MAJOR <= 4
#if OCAML_VERSION_MINOR < 9
#define caml_modify_generational_global_root(r,v) *r = v
#define caml_register_generational_global_root(r) caml_register_global_root(r)
#endif
#endif

/*** Callback utilities ***/

/* pointer to value should be registered to the GC by the caller */
void protect_callback(char *name, value *f, value *v1)
{
  if (*f == Val_unit) return;
  caml_acquire_runtime_system();
  {
    value vf = *f; value vv1 = *v1;
    CAMLparam1(vv1);
    if(Is_exception_result(caml_callback_exn(vf, vv1)))
      fprintf(stderr, "Egl.main_loop: "
	      "WARNING: %s raised an exception\n", name) ;
    CAMLdrop;
  }
  caml_release_runtime_system();
}

void protect_callback2(char *name, value *f, value *v1, value *v2)
{
  if (*f == Val_unit) return;
  caml_acquire_runtime_system();
  {
    value vf = *f; value vv1 = *v1; value vv2 = *v2;
    CAMLparam2(vv1,vv2);
    value r = caml_callback2_exn(vf, vv1, vv2);
    if(Is_exception_result(r))
      fprintf(stderr, "Egl.main_loop: "
	      "WARNING: %s raised exception\n", name) ;
    CAMLdrop;
  }
  caml_release_runtime_system();
}

void protect_callback3(char *name, value *f, value *v1,
			      value *v2, value *v3)
{
  if (*f == Val_unit) return;
  caml_acquire_runtime_system();
  {
    value vf = *f; value vv1 = *v1; value vv2 = *v2;
    value vv3 = *v3;
    CAMLparam3(vv1,vv2,vv3);
    if(Is_exception_result(caml_callback3_exn(vf, vv1, vv2, vv3)))
      fprintf(stderr, "Egl.main_loop: "
	      "WARNING: %s raised an exception\n", name) ;
    CAMLdrop;
  }
  caml_release_runtime_system();
}

void protect_callback4(char *name, value *f, value *v1,
			      value *v2, value *v3, value *v4)
{
  if (*f == Val_unit) return;
  caml_acquire_runtime_system();
  {
    value vf = *f; value vv1 = *v1; value vv2 = *v2;
    value vv3 = *v3; value vv4 = *v4;
    CAMLparam4(vv1,vv2,vv3,vv4);
    CAMLlocalN(tmp,4)
    tmp[0] = vv1; tmp[1]=vv2; tmp[2]=vv3; tmp[3]=vv4 ;
    value r = caml_callbackN_exn(vf, 4, tmp);
    if(Is_exception_result(r))
      fprintf(stderr, "Egl.main_loop: "
	      "WARNING: %s raised exception", name) ;
    CAMLdrop;
  }
  caml_release_runtime_system();
}


void egl_platform_lost(egl_context ctxt) {
  if(ctxt->delete_callback != Val_unit) {
    caml_callback(ctxt->delete_callback, Val_unit) ;
  }
  caml_acquire_runtime_system();
  caml_modify_generational_global_root(&(ctxt->idle_callback),
				       Val_unit) ;
  caml_modify_generational_global_root(&(ctxt->reshape_callback),
				       Val_unit) ;
  caml_modify_generational_global_root(&(ctxt->delete_callback),
				       Val_unit) ;
  caml_modify_generational_global_root(&(ctxt->key_press_callback),
				       Val_unit) ;
  caml_modify_generational_global_root(&(ctxt->key_release_callback),
				       Val_unit) ;
  caml_modify_generational_global_root(&(ctxt->button_press_callback),
				       Val_unit) ;
  caml_modify_generational_global_root(&(ctxt->button_release_callback),
				       Val_unit) ;
  caml_modify_generational_global_root(&(ctxt->motion_notify_callback),
				       Val_unit) ;
  caml_release_runtime_system();
  ctxt->context = EGL_NO_CONTEXT ;
  ctxt->surface = EGL_NO_SURFACE ;
  ctxt->display = EGL_NO_DISPLAY ;

  ctxt->width = ctxt->height = 0 ;
  ctxt->main_loop_continue = 0 ;
  ctxt->initialized = 0 ;
}

/****************************************************************************/
/*   TERMINATION                                                            */
/****************************************************************************/

static void free_resources(egl_context ctxt)
{
  if (!ctxt->initialized) return;
  if(ctxt->display != EGL_NO_DISPLAY) {
    eglMakeCurrent(ctxt->display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT) ;
    if(ctxt->surface != EGL_NO_SURFACE)
      eglDestroySurface(ctxt->display, ctxt->surface) ;
    if(ctxt->context != EGL_NO_CONTEXT)
      eglDestroyContext(ctxt->display, ctxt->context) ;
    eglTerminate(ctxt->display) ;
  }
  free_platform_ressources(ctxt);
  caml_remove_generational_global_root(&(ctxt->idle_callback));
  caml_remove_generational_global_root(&(ctxt->reshape_callback));
  caml_remove_generational_global_root(&(ctxt->delete_callback));
  caml_remove_generational_global_root(&(ctxt->key_press_callback));
  caml_remove_generational_global_root(&(ctxt->key_release_callback));
  caml_remove_generational_global_root(&(ctxt->button_press_callback));
  caml_remove_generational_global_root(&(ctxt->button_release_callback));
  caml_remove_generational_global_root(&(ctxt->motion_notify_callback));
  ctxt->main_loop_continue = 0 ;
  ctxt->initialized = 0 ;
}

void free_egl_context(value v) {
  free_resources(Val_ctxt(v));
}

int compare_egl_context(value v1, value v2) {
  egl_context c1 = Val_ctxt(v1);
  egl_context c2 = Val_ctxt(v2);
  if(!c1->initialized || !c2->initialized)
    caml_failwith("Egl.compare: not initialized") ;
  EGLSurface s1 = c1->surface;
  EGLSurface s2 = c2->surface;
  if (s1 < s2) return -1;
  if (s1 > s2) return 1;
  return 0;
}
intnat hash_egl_context(value v) {
  egl_context c = Val_ctxt(v);
  if(!c->initialized)
    caml_failwith("Egl.hash: not initialized") ;
  return (intnat)c->surface;
}

static struct custom_operations ops = {
    "egl_context",
    free_egl_context,
    compare_egl_context,
    hash_egl_context,
    custom_serialize_default,
    custom_deserialize_default,
    custom_fixed_length_default
};

CAMLprim value ml_egl_terminate(value v)
{
  CAMLparam1(v);
  egl_context ctxt = Val_ctxt(v);
  if(!ctxt->initialized)
    caml_failwith("Egl.terminate: not initialized") ;
  if (atomic_load(&ctxt->main_loop_reentrant))
    caml_failwith("Egl.terminate: while main loop running") ;

  free_resources(ctxt) ;
  CAMLreturn(Val_unit);
}

/****************************************************************************/
/*   INITIALIZATION                                                         */
/****************************************************************************/

void init_fail(egl_context ctxt, const char* s) {
  free_resources(ctxt);
  char *msg;
  asprintf(&msg, "Egl.initialize: %s", s);
  caml_failwith(msg);
}
value malloc_egl_context() {
  egl_context ctxt = malloc(sizeof(struct egl_context_struct));
  if (!ctxt) caml_failwith("fail to allocate context");

  value vctxt = caml_alloc_custom(&ops, sizeof(egl_context), 0, 1);
  Val_ctxt(vctxt) = ctxt;

  ctxt->display = EGL_NO_DISPLAY;
  ctxt->surface = EGL_NO_SURFACE;
  ctxt->context = EGL_NO_CONTEXT;
  ctxt->config = NULL;
  ctxt->width = ctxt->height = 0;
  atomic_init(&ctxt->main_loop_reentrant, 0) ;
  ctxt->main_loop_continue = 0;
  ctxt->idle_callback = Val_unit;
  ctxt->reshape_callback = Val_unit;
  ctxt->delete_callback = Val_unit;
  ctxt->key_press_callback = Val_unit;
  ctxt->key_release_callback = Val_unit;
  ctxt->button_press_callback = Val_unit;
  ctxt->button_release_callback = Val_unit;
  ctxt->motion_notify_callback = Val_unit;
  caml_register_generational_global_root(&(ctxt->idle_callback) );
  caml_register_generational_global_root(&(ctxt->reshape_callback)) ;
  caml_register_generational_global_root(&(ctxt->delete_callback)) ;
  caml_register_generational_global_root(&(ctxt->key_press_callback)) ;
  caml_register_generational_global_root(&(ctxt->key_release_callback)) ;
  caml_register_generational_global_root(&(ctxt->button_press_callback)) ;
  caml_register_generational_global_root(&(ctxt->button_release_callback)) ;
  caml_register_generational_global_root(&(ctxt->motion_notify_callback));
  ctxt->platform = malloc_platform_context(ctxt);
  return vctxt;
}

void showConfig(egl_context ctxt) {
  EGLint red_size, green_size, blue_size, alpha_size ;
  EGLint depth_size, stencil_size, samples ;
  EGLConfig config = ctxt->config;
  EGLDisplay display = ctxt->display;
  eglGetConfigAttrib(display, config, EGL_RED_SIZE, &red_size) ;
  eglGetConfigAttrib(display, config, EGL_GREEN_SIZE, &green_size) ;
  eglGetConfigAttrib(display, config, EGL_BLUE_SIZE, &blue_size) ;
  eglGetConfigAttrib(display, config, EGL_ALPHA_SIZE, &alpha_size) ;
  eglGetConfigAttrib(display, config, EGL_DEPTH_SIZE, &depth_size) ;
  eglGetConfigAttrib(display, config, EGL_STENCIL_SIZE, &stencil_size) ;
  eglGetConfigAttrib(display, config, EGL_SAMPLES, &samples) ;
  printf("RED:%d, GREEN:%d, BLUE:%d, ALPHA:%d, DEPTH:%d, STENCIL:%d, SAMPLES:%d\n",
	 red_size, green_size, blue_size, alpha_size, depth_size,
	 stencil_size, samples);
  fflush(stdout);
}

CAMLprim value ml_egl_initialize(value vc, value vw, value vh, value vn)
{
  CAMLparam4(vc, vw, vh, vn) ;
  CAMLlocal1(vctxt);
  vctxt = malloc_egl_context();
  EGLint attribs[20] ;

  egl_context ctxt = Val_ctxt(vctxt);

  ctxt->width = Int_val(vw) ;
  ctxt->height = Int_val(vh) ;
  init_platform_ressources(ctxt, String_val(vn));

  /* Open EGL Display */
  if((ctxt->display = eglGetDisplay(ctxt->platform_display)) == EGL_NO_DISPLAY)
    init_fail(ctxt, "cannot open EGL display") ;

  /* Initialize EGL Display */
  if(!eglInitialize(ctxt->display, NULL, NULL))
    init_fail(ctxt, "Egl.initialize: cannot initialize EGL display") ;

  /* Choose EGL Config */
  attribs[0] = EGL_COLOR_BUFFER_TYPE ;
  attribs[1] = EGL_RGB_BUFFER ;
  attribs[2] = EGL_RED_SIZE ;
  attribs[3] = Int_val(Field(vc, 0)) ;
  attribs[4] = EGL_GREEN_SIZE ;
  attribs[5] = Int_val(Field(vc, 1)) ;
  attribs[6] = EGL_BLUE_SIZE ;
  attribs[7] = Int_val(Field(vc, 2)) ;
  attribs[8] = EGL_ALPHA_SIZE ;
  attribs[9] = Int_val(Field(vc, 3)) ;
  attribs[10] = EGL_DEPTH_SIZE ;
  attribs[11] = Int_val(Field(vc, 4)) ;
  attribs[12] = EGL_STENCIL_SIZE ;
  attribs[13] = Int_val(Field(vc, 5)) ;
  attribs[14] = EGL_SAMPLES ;
  attribs[15] = Int_val(Field(vc, 6)) ;
  attribs[16] = EGL_SAMPLE_BUFFERS ;
  attribs[17] = (attribs[15] == 0 ? 0 : 1) ;
  attribs[18] = EGL_NONE ;
  int nconf ;
  if(!eglChooseConfig(ctxt->display, attribs, NULL, 0, &nconf) || nconf < 1)
    init_fail(ctxt, "cannot choose EGL config") ;
  EGLConfig *configs = (EGLConfig*) malloc( nconf * sizeof(EGLConfig));
  if(!eglChooseConfig(ctxt->display, attribs, configs, nconf, &nconf) || nconf < 1)
    init_fail(ctxt, "cannot choose EGL config") ;
  int best_i = 0;
  EGLint best_depth, best_samples;
  eglGetConfigAttrib(ctxt->display, configs[0], EGL_DEPTH_SIZE, &best_depth) ;
  eglGetConfigAttrib(ctxt->display, configs[0], EGL_SAMPLES, &best_samples) ;
  for(int i=1;i<nconf;i++) {
    EGLint depth, samples;
    eglGetConfigAttrib(ctxt->display, configs[i], EGL_DEPTH_SIZE, &depth) ;
    eglGetConfigAttrib(ctxt->display, configs[i], EGL_SAMPLES, &samples) ;
    if (depth > best_depth) {
      best_i = i; best_depth = depth; best_samples = samples;
    }
    else if (depth == best_depth && samples > best_samples) {
      best_i = i; best_samples = samples;
    }
  }

  bcopy(&(configs[best_i]),&(ctxt->config),sizeof(EGLConfig));
  free(configs); showConfig(ctxt);
  /* Create EGL Surface */
  ctxt->surface = eglCreateWindowSurface(ctxt->display,
					 ctxt->config,
					 ctxt->platform_window,
					 NULL) ;
  if(ctxt->surface == EGL_NO_SURFACE)
    init_fail(ctxt,"cannot create EGL surface") ;

  /* Binding the API */
  if(!eglBindAPI(EGL_OPENGL_ES_API))
    init_fail(ctxt, "cannot bind OpenGL ES API") ;

  /* Create EGL context */
  attribs[0] = EGL_CONTEXT_CLIENT_VERSION ;
  attribs[1] = 2 ;
  attribs[2] = EGL_NONE ;
  ctxt->context = eglCreateContext(ctxt->display,
				   ctxt->config,
				   EGL_NO_CONTEXT,
				   attribs) ;
  if(ctxt->context == EGL_NO_CONTEXT)
    init_fail(ctxt, "cannot create EGL context") ;

  /* Binding context with surface */
  if(!eglMakeCurrent(ctxt->display,
		     ctxt->surface,
		     ctxt->surface,
		     ctxt->context))
    init_fail(ctxt, "cannot bind EGL surface to context") ;

  ctxt->initialized = 1 ;
  CAMLreturn(vctxt);
}

/****************************************************************************/
/*   SETTING CALLBACKS                                                      */
/****************************************************************************/

CAMLprim value ml_egl_set_idle_callback(value vc, value v)
{
  CAMLparam2(vc, v) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.set_idle_callback: not initialized") ;
  caml_modify_generational_global_root(&(ctxt->idle_callback), v) ;
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_unset_idle_callback(value vc)
{
  CAMLparam1(vc) ;
  egl_context ctxt = Val_ctxt(vc);
   if(!ctxt->initialized)
    caml_failwith("Egl.set_idle_callback: not initialized") ;
   caml_modify_generational_global_root(&(ctxt->idle_callback), Val_unit);
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_set_reshape_callback(value vc, value v)
{
  CAMLparam2(vc,v) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.set_reshape_callback: not initialized") ;
  caml_modify_generational_global_root(&(ctxt->reshape_callback), v) ;
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_set_delete_callback(value vc, value v)
{
  CAMLparam2(vc,v) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.set_delete_callback: not initialized") ;
  caml_modify_generational_global_root(&(ctxt->delete_callback), v) ;
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_set_key_press_callback(value vc, value v)
{
  CAMLparam2(vc, v) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.set_key_press_callback: not initialized") ;
  caml_modify_generational_global_root(&(ctxt->key_press_callback), v) ;
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_set_key_release_callback(value vc, value v)
{
  CAMLparam2(vc, v) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.set_key_release_callback: not initialized") ;
  caml_modify_generational_global_root(&(ctxt->key_release_callback), v) ;
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_set_button_press_callback(value vc, value v)
{
  CAMLparam2(vc,v) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.set_button_press_callback: not initialized") ;
  caml_modify_generational_global_root(&(ctxt->button_press_callback), v) ;
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_set_button_release_callback(value vc, value v)
{
  CAMLparam2(vc,v) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.set_button_release_callback: not initialized") ;
  caml_modify_generational_global_root(&(ctxt->button_release_callback), v) ;
  CAMLreturn(Val_unit) ;
}


CAMLprim value ml_egl_set_motion_notify_callback(value vc, value v)
{
  CAMLparam2(vc,v) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.set_motion_notify_callback: not initialized") ;
  caml_modify_generational_global_root(&(ctxt->motion_notify_callback), v) ;
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_make_current(value vc) {
  CAMLparam1(vc);
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.make_xurrent: not initialized") ;
  if(!eglMakeCurrent(ctxt->display, ctxt->surface,
		     ctxt->surface, ctxt->context))
    caml_failwith("Can not set current context");
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_detach(value vc) {
  CAMLparam1(vc);
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.detach: not initialized") ;
  if(!eglMakeCurrent(ctxt->display, ctxt->surface,
 		                        ctxt->surface, ctxt->context))
    caml_failwith("Can not set current context");
  CAMLreturn(Val_unit) ;
}

CAMLprim value ml_egl_exit_loop(value vc)
{
  CAMLparam1(vc);
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.exit_loop: not initialized") ;
  if(!ctxt->main_loop_reentrant)
    caml_failwith("Egl.exit_loop: can only be called inside main_loop\n") ;
  ctxt->main_loop_continue = 0 ;
  CAMLreturn(Val_unit) ;
}

/****************************************************************************/
/*   MISCELLANEOUS                                                          */
/****************************************************************************/

CAMLprim value ml_egl_swap_buffers(value vc)
{
  CAMLparam1(vc) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized) caml_failwith("Egl.swap_buffers: not initialized") ;
  eglSwapBuffers(ctxt->display, ctxt->surface) ;
  CAMLreturn(Val_unit);
}

CAMLprim value ml_egl_query_version(value vc)
{
  CAMLparam1(vc) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.query_version: not initialized") ;
  const char *s = eglQueryString(ctxt->display, EGL_VERSION) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_vendor(value vc)
{
  CAMLparam1(vc) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.query_vendor: not initialized") ;
  const char *s = eglQueryString(ctxt->display, EGL_VENDOR) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_extensions(value vc)
{
  CAMLparam1(vc) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.query_extensions: not initialized") ;
  const char *s = eglQueryString(ctxt->display, EGL_EXTENSIONS) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_client_apis(value vc)
{
  CAMLparam1(vc) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.query_client_apis: not initialized") ;
  const char *s = eglQueryString(ctxt->display, EGL_CLIENT_APIS) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_config(value vc)
{
  CAMLparam1(vc) ;
  egl_context ctxt = Val_ctxt(vc);
  if(!ctxt->initialized)
    caml_failwith("Egl.query_config: not initialized") ;
  CAMLlocal1(ret) ;
  EGLint red_size, green_size, blue_size, alpha_size ;
  EGLint depth_size, stencil_size, samples ;
  eglGetConfigAttrib(ctxt->display, ctxt->config, EGL_RED_SIZE, &red_size) ;
  eglGetConfigAttrib(ctxt->display, ctxt->config, EGL_GREEN_SIZE, &green_size) ;
  eglGetConfigAttrib(ctxt->display, ctxt->config, EGL_BLUE_SIZE, &blue_size) ;
  eglGetConfigAttrib(ctxt->display, ctxt->config, EGL_ALPHA_SIZE, &alpha_size) ;
  eglGetConfigAttrib(ctxt->display, ctxt->config, EGL_DEPTH_SIZE, &depth_size) ;
  eglGetConfigAttrib(ctxt->display, ctxt->config, EGL_STENCIL_SIZE, &stencil_size) ;
  eglGetConfigAttrib(ctxt->display, ctxt->config, EGL_SAMPLES, &samples) ;
  ret = caml_alloc_tuple(7) ;
  Store_field(ret, 0, Val_int(red_size)) ;
  Store_field(ret, 1, Val_int(green_size)) ;
  Store_field(ret, 2, Val_int(blue_size)) ;
  Store_field(ret, 3, Val_int(alpha_size)) ;
  Store_field(ret, 4, Val_int(depth_size)) ;
  Store_field(ret, 5, Val_int(stencil_size)) ;
  Store_field(ret, 6, Val_int(samples)) ;
  CAMLreturn(ret) ;
}
