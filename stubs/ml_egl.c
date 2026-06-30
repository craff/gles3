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

int initialized = 0 ;

NativeDisplayType platform_display;
NativeWindowType  platform_window;
int width, height ;
static EGLDisplay display = EGL_NO_DISPLAY ;
static EGLConfig config ;
static EGLSurface surface = EGL_NO_SURFACE ;
static EGLContext context = EGL_NO_CONTEXT ;
int main_loop_reentrant = 0 ;
int main_loop_continue = 0 ;

/* Callbacks */

value default_callback = Val_unit ;
value idle_callback = Val_unit ;
value reshape_callback = Val_unit ;
value delete_callback = Val_unit ;
value key_press_callback = Val_unit ;
value key_release_callback = Val_unit ;
value button_press_callback = Val_unit ;
value button_release_callback = Val_unit ;
value motion_notify_callback = Val_unit ;

/*** Callback utilities ***/

/* pointer to value should be registered to the GC by the caller */
void protect_callback(char *name, value *f, value *v1)
{
  if (!f) return;
  caml_acquire_runtime_system();
  {
    CAMLparam0();
    if(Is_exception_result(caml_callback_exn(*f, *v1)))
      fprintf(stderr, "Egl.main_loop: "
	      "WARNING: %s raised an exception\n", name) ;
    CAMLdrop;
  }
  caml_release_runtime_system();
}

void protect_callback2(char *name, value *f, value *v1, value *v2)
{
  if (!f) return;
  caml_acquire_runtime_system();
  {
    CAMLparam0();
    if(Is_exception_result(caml_callback2_exn(*f, *v1, *v2)))
      fprintf(stderr, "Egl.main_loop: "
	      "WARNING: %s raised an exception\n", name) ;
    CAMLdrop;
  }
  caml_release_runtime_system();
}

void protect_callback3(char *name, value *f, value *v1,
			      value *v2, value *v3)
{
  if (!f) return;
  caml_acquire_runtime_system();
  {
    CAMLparam0();
    if(Is_exception_result(caml_callback3_exn(*f, *v1, *v2, *v3)))
      fprintf(stderr, "Egl.main_loop: "
	      "WARNING: %s raised an exception\n", name) ;
    CAMLdrop;
  }
  caml_release_runtime_system();
}

void protect_callback4(char *name, value *f, value *v1,
			      value *v2, value *v3, value *v4)
{
  if (!f) return;
  caml_acquire_runtime_system();
  {
    CAMLparam0();
    CAMLlocalN(tmp,4);
    tmp[0] = *v1; tmp[1]=*v2; tmp[2]=*v3; tmp[3]=*v4 ;
    if(Is_exception_result(caml_callbackN_exn(*f, 4, tmp)))
      fprintf(stderr, "Egl.main_loop: "
	      "WARNING: %s raised an exception\n", name) ;
    CAMLdrop;
  }
  caml_release_runtime_system();
}


void egl_platform_lost(void) {
  value saved_delete_callback = delete_callback ;

  caml_modify_generational_global_root(&idle_callback, default_callback) ;
  caml_modify_generational_global_root(&reshape_callback, default_callback) ;
  caml_modify_generational_global_root(&delete_callback, default_callback) ;
  caml_modify_generational_global_root(&key_press_callback, default_callback) ;
  caml_modify_generational_global_root(&key_release_callback, default_callback) ;
  caml_modify_generational_global_root(&button_press_callback, default_callback) ;
  caml_modify_generational_global_root(&button_release_callback, default_callback) ;
  caml_modify_generational_global_root(&motion_notify_callback, default_callback) ;

  context = EGL_NO_CONTEXT ;
  surface = EGL_NO_SURFACE ;
  display = EGL_NO_DISPLAY ;

  width = height = 0 ;
  main_loop_reentrant = 0 ;
  main_loop_continue = 0 ;
  initialized = 0 ;

  if(saved_delete_callback != (value)NULL) {
    caml_callback(saved_delete_callback, Val_unit) ;
    exit(1) ;
  }

}

/****************************************************************************/
/*   TERMINATION                                                            */
/****************************************************************************/

static void free_resources()
{
  if(display != EGL_NO_DISPLAY)
    eglMakeCurrent(display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT) ;
  if(surface != EGL_NO_SURFACE) {
    if(display != EGL_NO_DISPLAY)
      eglDestroySurface(display, surface) ;
    surface = EGL_NO_SURFACE ;
  }
  if(context != EGL_NO_CONTEXT) {
    if(display != EGL_NO_DISPLAY)
      eglDestroyContext(display, context) ;
    context = EGL_NO_CONTEXT ;
  }
  if(display != EGL_NO_DISPLAY) {
    eglTerminate(display) ;
    display = EGL_NO_DISPLAY ;
  }
  free_platform_ressources();
  width = height = 0 ;
  main_loop_reentrant = 0 ;
  main_loop_continue = 0 ;
  initialized = 0 ;
}

void ml_egl_terminate()
{
  if(!initialized)
    caml_failwith("Egl.terminate: not initialized") ;
  free_resources() ;
  initialized = 0 ; /* already done above */
}

/****************************************************************************/
/*   INITIALIZATION                                                         */
/****************************************************************************/

void init_fail(const char* s) {
  free_resources();
  char *msg;
  asprintf(&msg, "Egl.initialize: %s", s);
  caml_failwith(msg);
}

void showConfig(EGLConfig *config) {
  EGLint red_size, green_size, blue_size, alpha_size ;
  EGLint depth_size, stencil_size, samples ;
  eglGetConfigAttrib(display, *config, EGL_RED_SIZE, &red_size) ;
  eglGetConfigAttrib(display, *config, EGL_GREEN_SIZE, &green_size) ;
  eglGetConfigAttrib(display, *config, EGL_BLUE_SIZE, &blue_size) ;
  eglGetConfigAttrib(display, *config, EGL_ALPHA_SIZE, &alpha_size) ;
  eglGetConfigAttrib(display, *config, EGL_DEPTH_SIZE, &depth_size) ;
  eglGetConfigAttrib(display, *config, EGL_STENCIL_SIZE, &stencil_size) ;
  eglGetConfigAttrib(display, *config, EGL_SAMPLES, &samples) ;
  printf("RED:%d, GREEN:%d, BLUE:%d, ALPHA:%d, DEPTH:%d, STENCIL:%d, SAMPLES:%d\n",
	 red_size, green_size, blue_size, alpha_size, depth_size, stencil_size, samples);
  fflush(stdout);
}
CAMLprim value ml_egl_initialize(value vf, value vc, value vw, value vh, value vn)
{
  CAMLparam5(vf, vc, vw, vh, vn) ;

  EGLint attribs[20] ;

  if(initialized)
    init_fail("already initialized") ;

  width = Int_val(vw) ;
  height = Int_val(vh) ;
  init_platform_ressources(width, height, String_val(vn));

  /* Open EGL Display */
  if((display = eglGetDisplay(platform_display)) == EGL_NO_DISPLAY)
    init_fail("cannot open EGL display") ;

  /* Initialize EGL Display */
  if(!eglInitialize(display, NULL, NULL))
    init_fail("Egl.initialize: cannot initialize EGL display") ;

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
  if(!eglChooseConfig(display, attribs, NULL, 0, &nconf) || nconf < 1)
    init_fail("cannot choose EGL config") ;
  printf("num conf %d\n",nconf); fflush(stdout);
  EGLConfig *configs = (EGLConfig*) malloc( nconf * sizeof(EGLConfig));
  if(!eglChooseConfig(display, attribs, configs, nconf, &nconf) || nconf < 1)
    init_fail("cannot choose EGL config") ;
  int best_i = 0;
  EGLint best_depth, best_samples;
  eglGetConfigAttrib(display, configs[0], EGL_DEPTH_SIZE, &best_depth) ;
  eglGetConfigAttrib(display, configs[0], EGL_SAMPLES, &best_samples) ;
  for(int i=1;i<nconf;i++) {
    EGLint depth, samples;
    eglGetConfigAttrib(display, configs[i], EGL_DEPTH_SIZE, &depth) ;
    eglGetConfigAttrib(display, configs[i], EGL_SAMPLES, &samples) ;
    if (depth > best_depth) {
      best_i = i; best_depth = depth; best_samples = samples;
    }
    else if (depth == best_depth && samples > best_samples) {
      best_i = i; best_samples = samples;
    }
  }

  bcopy(&(configs[best_i]),&config,sizeof(EGLConfig));
  free(configs); showConfig(&config);
  /* Create EGL Surface */
  surface = eglCreateWindowSurface(display, config, platform_window, NULL) ;
  if(surface == EGL_NO_SURFACE)
    init_fail("cannot create EGL surface") ;

  /* Binding the API */
  if(!eglBindAPI(EGL_OPENGL_ES_API))
    init_fail("cannot bind OpenGL ES API") ;

  /* Create EGL context */
  attribs[0] = EGL_CONTEXT_CLIENT_VERSION ;
  attribs[1] = 2 ;
  attribs[2] = EGL_NONE ;
  context = eglCreateContext(display, config, EGL_NO_CONTEXT, attribs) ;
  if(context == EGL_NO_CONTEXT)
    init_fail("cannot create EGL context") ;

  /* Binding context with surface */
  if(!eglMakeCurrent(display, surface, surface, context))
    init_fail("cannot bind EGL surface to context") ;

  /* Register CAML roots for callbacks */
  default_callback = vf;
  idle_callback = vf;
  reshape_callback = vf;
  delete_callback = vf;
  key_press_callback = vf;
  key_release_callback = vf;
  button_press_callback = vf;
  button_release_callback = vf;
  motion_notify_callback = vf;

  caml_register_generational_global_root(&default_callback) ;
  caml_register_generational_global_root(&idle_callback) ;
  caml_register_generational_global_root(&reshape_callback) ;
  caml_register_generational_global_root(&delete_callback) ;
  caml_register_generational_global_root(&key_press_callback) ;
  caml_register_generational_global_root(&key_release_callback) ;
  caml_register_generational_global_root(&button_press_callback) ;
  caml_register_generational_global_root(&button_release_callback) ;
  caml_register_generational_global_root(&motion_notify_callback) ;

  initialized = 1 ;

  CAMLreturn(Val_unit) ;
}

/****************************************************************************/
/*   SETTING CALLBACKS                                                      */
/****************************************************************************/

void ml_egl_set_idle_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    caml_failwith("Egl.set_idle_callback: not initialized") ;
  caml_modify_generational_global_root(&idle_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_unset_idle_callback()
{
  if(!initialized)
    caml_failwith("Egl.set_idle_callback: not initialized") ;
  caml_modify_generational_global_root(&idle_callback, default_callback) ;
}

void ml_egl_set_reshape_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    caml_failwith("Egl.set_reshape_callback: not initialized") ;
  caml_modify_generational_global_root(&reshape_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_set_delete_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    caml_failwith("Egl.set_delete_callback: not initialized") ;
  caml_modify_generational_global_root(&delete_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_set_key_press_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    caml_failwith("Egl.set_key_press_callback: not initialized") ;
  caml_modify_generational_global_root(&key_press_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_set_key_release_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    caml_failwith("Egl.set_key_release_callback: not initialized") ;
  caml_modify_generational_global_root(&key_release_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_set_button_press_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    caml_failwith("Egl.set_button_press_callback: not initialized") ;
  caml_modify_generational_global_root(&button_press_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_set_button_release_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    caml_failwith("Egl.set_button_release_callback: not initialized") ;
  caml_modify_generational_global_root(&button_release_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_make_current() {
  CAMLparam0();
  if(!eglMakeCurrent(display, surface, surface, context))
    caml_failwith("Can not set current context");
  CAMLreturn0 ;
}


void ml_egl_set_motion_notify_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    caml_failwith("Egl.set_motion_notify_callback: not initialized") ;
  caml_modify_generational_global_root(&motion_notify_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_exit_loop()
{
  if(!main_loop_reentrant)
    caml_failwith("Egl.exit_loop: can only be called inside main_loop\n") ;
  main_loop_continue = 0 ;
}

/****************************************************************************/
/*   MISCELLANEOUS                                                          */
/****************************************************************************/

void ml_egl_swap_buffers()
{
  if(!initialized) caml_failwith("Egl.swap_buffers: not initialized") ;
  eglSwapBuffers(display, surface) ;
}

CAMLprim value ml_egl_query_version()
{
  CAMLparam0() ;
  if(!initialized)
    caml_failwith("Egl.query_version: not initialized") ;
  const char *s = eglQueryString(display, EGL_VERSION) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_vendor()
{
  CAMLparam0() ;
  if(!initialized)
    caml_failwith("Egl.query_vendor: not initialized") ;
  const char *s = eglQueryString(display, EGL_VENDOR) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_extensions()
{
  CAMLparam0() ;
  if(!initialized)
    caml_failwith("Egl.query_extensions: not initialized") ;
  const char *s = eglQueryString(display, EGL_EXTENSIONS) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_client_apis()
{
  CAMLparam0() ;
  if(!initialized)
    caml_failwith("Egl.query_client_apis: not initialized") ;
  const char *s = eglQueryString(display, EGL_CLIENT_APIS) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_config()
{
  CAMLparam0() ;
  CAMLlocal1(ret) ;
  if(!initialized)
    caml_failwith("Egl.query_config: not initialized") ;
  EGLint red_size, green_size, blue_size, alpha_size ;
  EGLint depth_size, stencil_size, samples ;
  eglGetConfigAttrib(display, config, EGL_RED_SIZE, &red_size) ;
  eglGetConfigAttrib(display, config, EGL_GREEN_SIZE, &green_size) ;
  eglGetConfigAttrib(display, config, EGL_BLUE_SIZE, &blue_size) ;
  eglGetConfigAttrib(display, config, EGL_ALPHA_SIZE, &alpha_size) ;
  eglGetConfigAttrib(display, config, EGL_DEPTH_SIZE, &depth_size) ;
  eglGetConfigAttrib(display, config, EGL_STENCIL_SIZE, &stencil_size) ;
  eglGetConfigAttrib(display, config, EGL_SAMPLES, &samples) ;
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
