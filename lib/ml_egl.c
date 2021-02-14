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
/* along with MLGles3.  If not, see <http://www.gnu.org/licenses/>.         */
/****************************************************************************/
/* ml_gles3x.c: ML stubs for Gles3x companion library                       */
/****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
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
#include <unistd.h>

#if OCAML_VERSION_MAJOR <= 4
#if OCAML_VERSION_MINOR < 9
#define caml_modify_generational_global_root(r,v) *r = v
#define caml_register_generational_global_root(r) caml_register_global_root(r)
#endif
#endif

static int initialized = 0 ;

static Display *xdisplay = NULL ;
static Window xwindow = None ;
static int width, height ;
static int (*saved_IOErrorHandler)(Display *) = NULL ;
static EGLDisplay display = EGL_NO_DISPLAY ;
static EGLConfig config ;
static EGLSurface surface = EGL_NO_SURFACE ;
static EGLContext context = EGL_NO_CONTEXT ;
static int main_loop_reentrant = 0 ;
static int main_loop_continue = 0 ;

/* Callbacks */

long XEventMask = StructureNotifyMask; // Always have this, to always receive delete
static value default_callback = Val_unit ;
static value idle_callback = Val_unit ;
static value reshape_callback = Val_unit ;
static value delete_callback = Val_unit ;
static value key_press_callback = Val_unit ;
static value key_release_callback = Val_unit ;
static value button_press_callback = Val_unit ;
static value button_release_callback = Val_unit ;
static value motion_notify_callback = Val_unit ;
static value enter_window_callback = Val_unit ;
static value leave_window_callback = Val_unit ;
static value change_focus_callback = Val_unit ;

/*** Callback utilities ***/

/* pointer to value should be registered to the GC by the caller */
static void protect_callback(char *name, value *f, value *v1)
{
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

static void protect_callback2(char *name, value *f, value *v1, value *v2)
{
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

static void protect_callback3(char *name, value *f, value *v1,
			      value *v2, value *v3)
{
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

static void protect_callback4(char *name, value *f, value *v1,
			      value *v2, value *v3, value *v4)
{
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

/****************************************************************************/
/*   X IO ERROR HANDLING                                                    */
/****************************************************************************/

static int IOErrorHandler(Display *dpy)
{
  printf("IOErrorHandler \n"); fflush(stdout);
  /* If error came from another display, we call saved_IOErrorHandler */
  if(dpy != xdisplay) {
    if(saved_IOErrorHandler != NULL)
      (*saved_IOErrorHandler)(dpy) ;
    return 0 ;
  }

  /* X connection has been lost, we cannot free allocated resources.
     Instead, we try to restore an uninitialized state. */

  value saved_delete_callback = delete_callback ;

  caml_modify_generational_global_root(&idle_callback, default_callback) ;
  caml_modify_generational_global_root(&reshape_callback, default_callback) ;
  caml_modify_generational_global_root(&delete_callback, default_callback) ;
  caml_modify_generational_global_root(&key_press_callback, default_callback) ;
  caml_modify_generational_global_root(&key_release_callback, default_callback) ;
  caml_modify_generational_global_root(&button_press_callback, default_callback) ;
  caml_modify_generational_global_root(&button_release_callback, default_callback) ;
  caml_modify_generational_global_root(&motion_notify_callback, default_callback) ;
  caml_modify_generational_global_root(&enter_window_callback, default_callback) ;
  caml_modify_generational_global_root(&leave_window_callback, default_callback) ;
  caml_modify_generational_global_root(&change_focus_callback, default_callback) ;

  context = EGL_NO_CONTEXT ;
  surface = EGL_NO_SURFACE ;
  display = EGL_NO_DISPLAY ;

  xwindow = None ;
  width = height = 0 ;
  xdisplay = NULL ;

  XSetIOErrorHandler(saved_IOErrorHandler) ;
  saved_IOErrorHandler = NULL ;
  main_loop_reentrant = 0 ;
  main_loop_continue = 0 ;
  initialized = 0 ;

  if(saved_delete_callback != (value)NULL) {
    caml_callback(saved_delete_callback, Val_unit) ;
    exit(1) ;
  }

  return 0 ;
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
  if(xwindow != None) {
    if(xdisplay != NULL)
      XDestroyWindow(xdisplay, xwindow) ;
    xwindow = None ;
  }
  if(xdisplay != NULL) {
    XCloseDisplay(xdisplay) ;
    xdisplay = NULL ;
  }
  if(saved_IOErrorHandler != NULL) {
    XSetIOErrorHandler(saved_IOErrorHandler) ;
    saved_IOErrorHandler = NULL ;
  }
  width = height = 0 ;
  main_loop_reentrant = 0 ;
  main_loop_continue = 0 ;
  initialized = 0 ;
}

void ml_egl_terminate()
{
  if(!initialized)
    failwith("Egl.terminate: not initialized") ;
  free_resources() ;
  initialized = 0 ; /* already done above */
}

/****************************************************************************/
/*   INITIALIZATION                                                         */
/****************************************************************************/

#define init_fail(s)  (free_resources(), failwith("Egl.initialize: " s))

CAMLprim value ml_egl_initialize(value vf, value vc, value vw, value vh, value vn)
{
  CAMLparam5(vf, vc, vw, vh, vn) ;

  EGLint attribs[20] ;

  if(initialized)
    init_fail("already initialized") ;

  /* Open X Display */
  if((xdisplay = XOpenDisplay(NULL)) == NULL)
    init_fail("cannot open X display") ;

  /* Open X Window */
  width = Int_val(vw) ;
  height = Int_val(vh) ;
  xwindow = XCreateSimpleWindow(xdisplay, DefaultRootWindow(xdisplay),
				0, 0, width, height, 0, 0, 0) ;
  if(xwindow == None)
    init_fail("cannot create X window") ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  XMapWindow(xdisplay, xwindow) ;
  XStoreName(xdisplay, xwindow, String_val(vn)) ;

  /* Open EGL Display */
  if((display = eglGetDisplay(xdisplay)) == EGL_NO_DISPLAY)
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
  if(!eglChooseConfig(display, attribs, &config, 1, &nconf) || nconf != 1)
    init_fail("cannot choose EGL config") ;

  /* Create EGL Surface */
  surface = eglCreateWindowSurface(display, config, xwindow, NULL) ;
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

  saved_IOErrorHandler = XSetIOErrorHandler(&IOErrorHandler) ;

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
  caml_register_generational_global_root(&enter_window_callback) ;
  caml_register_generational_global_root(&leave_window_callback) ;
  caml_register_generational_global_root(&change_focus_callback) ;

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
    failwith("Egl.set_idle_callback: not initialized") ;
  caml_modify_generational_global_root(&idle_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_unset_idle_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_idle_callback: not initialized") ;
  caml_modify_generational_global_root(&idle_callback, default_callback) ;
  CAMLreturn0;
}

void ml_egl_set_reshape_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_reshape_callback: not initialized") ;
  caml_modify_generational_global_root(&reshape_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_unset_reshape_callback(value v)
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_reshape_callback: not initialized") ;
  caml_modify_generational_global_root(&reshape_callback, default_callback) ;
  CAMLreturn0 ;
}

void ml_egl_set_delete_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_delete_callback: not initialized") ;
  caml_modify_generational_global_root(&delete_callback, v) ;
  CAMLreturn0 ;
}

void ml_egl_unset_delete_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_delete_callback: not initialized") ;
  caml_modify_generational_global_root(&delete_callback, default_callback) ;
  CAMLreturn0 ;
}

void ml_egl_set_key_press_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_key_press_callback: not initialized") ;
  caml_modify_generational_global_root(&key_press_callback, v) ;
  XEventMask = XEventMask | KeyPressMask;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_unset_key_press_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_key_press_callback: not initialized") ;
  caml_modify_generational_global_root(&key_press_callback, default_callback) ;
  XEventMask = XEventMask & ~KeyPressMask;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_set_key_release_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_key_release_callback: not initialized") ;
  caml_modify_generational_global_root(&key_release_callback, v) ;
  XEventMask = XEventMask | KeyReleaseMask;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_unset_key_release_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_key_release_callback: not initialized") ;
  caml_modify_generational_global_root(&key_release_callback, default_callback) ;
  XEventMask = XEventMask & ~KeyReleaseMask;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_set_button_press_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_button_press_callback: not initialized") ;
  caml_modify_generational_global_root(&button_press_callback, v) ;
  XEventMask = XEventMask | ButtonPressMask;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_unset_button_press_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_button_press_callback: not initialized") ;
  caml_modify_generational_global_root(&button_press_callback, default_callback) ;
  XEventMask = XEventMask & ~ButtonPressMask;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_set_button_release_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_button_release_callback: not initialized") ;
  caml_modify_generational_global_root(&button_release_callback, v) ;
  XEventMask = XEventMask | ButtonReleaseMask;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_unset_button_release_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_button_release_callback: not initialized") ;
  caml_modify_generational_global_root(&button_release_callback, default_callback) ;
  XEventMask = XEventMask & ~ButtonReleaseMask;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_set_motion_notify_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_motion_notify_callback: not initialized") ;
  caml_modify_generational_global_root(&motion_notify_callback, v) ;
  XEventMask = XEventMask | PointerMotionMask ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_unset_motion_notify_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_motion_notify_callback: not initialized") ;
  caml_modify_generational_global_root(&motion_notify_callback, default_callback) ;
  XEventMask = XEventMask & ~PointerMotionMask ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_set_enter_window_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_enter_window_callback: not initialized") ;
  caml_modify_generational_global_root(&enter_window_callback, v) ;
  XEventMask = XEventMask | EnterWindowMask ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_unset_enter_window_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_enter_window_callback: not initialized") ;
  caml_modify_generational_global_root(&enter_window_callback, default_callback) ;
  XEventMask = XEventMask & ~EnterWindowMask ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_set_leave_window_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_leave_window_callback: not initialized") ;
  caml_modify_generational_global_root(&leave_window_callback, v) ;
  XEventMask = XEventMask | LeaveWindowMask ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_unset_leave_window_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.unset_leave_window_callback: not initialized") ;
  caml_modify_generational_global_root(&leave_window_callback, default_callback) ;
  XEventMask = XEventMask & ~LeaveWindowMask ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_set_change_focus_callback(value v)
{
  CAMLparam1(v) ;
  if(!initialized)
    failwith("Egl.set_change_focus_callback: not initialized") ;
  caml_modify_generational_global_root(&change_focus_callback, v) ;
  XEventMask = XEventMask | FocusChangeMask ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

void ml_egl_unset_change_focus_callback()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.set_change_focus_callback: not initialized") ;
  caml_modify_generational_global_root(&change_focus_callback, default_callback) ;
  XEventMask = XEventMask & ~FocusChangeMask ;
  XSelectInput(xdisplay, xwindow, XEventMask);
  CAMLreturn0 ;
}

/****************************************************************************/
/*   MAIN LOOP                                                              */
/****************************************************************************/

void ml_egl_main_loop()
{
  CAMLparam0() ;
  XEvent event ;

  if(!initialized)
    failwith("Egl.main_loop: not initialized") ;

  if(main_loop_reentrant)
    failwith("Egl.main_loop: forbidden reentrant call") ;

  caml_release_runtime_system();

  main_loop_reentrant = 1 ;

  Atom wmDeleteMessage = XInternAtom(xdisplay, "WM_DELETE_WINDOW", False);
  XSetWMProtocols(xdisplay, xwindow, &wmDeleteMessage, 1);

  main_loop_continue = 1 ;

  while(main_loop_continue) {

    if(idle_callback != default_callback) {
      while(XPending(xdisplay) == 0) {
	value u = Val_unit;
	protect_callback("idle callback", &idle_callback, &u) ;
      }
    }
    XNextEvent(xdisplay, &event) ;

    switch(event.type) {
    case ConfigureNotify:
      if(event.xconfigure.display == xdisplay &&
	 event.xconfigure.window == xwindow &&
	 (event.xconfigure.width != width ||
	  event.xconfigure.height != height) &&
	 reshape_callback != default_callback)
	{
	  width = event.xconfigure.width ;
	  height = event.xconfigure.height ;
	  value ml_width = Val_int(width);
	  value ml_height = Val_int(height);
	  protect_callback2("reshape callback",
			    &reshape_callback,
			    &ml_width, &ml_height) ;
	}
      break ;
    case ClientMessage:
      if(event.xclient.display == xdisplay &&
	 event.xclient.window == xwindow &&
	 event.xclient.data.l[0] == wmDeleteMessage)
	{
	  if(delete_callback == default_callback)
	    main_loop_continue = 0 ;
	  else
	    {
	      value u = Val_unit;
	      protect_callback("delete callback", &delete_callback, &u) ;
	    }
	}
      break ;
    case KeyPress:
      if(event.xkey.display == xdisplay &&
	 event.xkey.window == xwindow &&
	 key_press_callback != default_callback)
	{
	  KeySym keysym ;
	  XLookupString(&(event.xkey), NULL, 0, &keysym, NULL) ;
	  value ml_keysym = Val_int(keysym);
	  value ml_state = Val_int(event.xkey.state);
	  value ml_x = Val_int(event.xkey.x);
	  value ml_y = Val_int(event.xkey.y);
	  protect_callback4("key press callback", &key_press_callback,
			    &ml_keysym, &ml_state, &ml_x, &ml_y);
	}
      break ;
    case KeyRelease:
      if(event.xkey.display == xdisplay &&
	 event.xkey.window == xwindow &&
	 key_release_callback != default_callback)
	{
	  KeySym keysym ;
	  XLookupString(&(event.xkey), NULL, 0, &keysym, NULL) ;
	  value ml_keysym = Val_int(keysym);
	  value ml_state = Val_int(event.xkey.state);
	  value ml_x = Val_int(event.xkey.x);
	  value ml_y = Val_int(event.xkey.y);
	  protect_callback4("key release callback", &key_release_callback,
			    &ml_keysym, &ml_state, &ml_x, &ml_y);
	}
      break ;
    case ButtonPress:
      if(event.xbutton.display == xdisplay &&
	 event.xbutton.window == xwindow &&
	 button_press_callback != default_callback)
	{
	  value ml_button = Val_int(event.xbutton.button - Button1);
	  value ml_state = Val_int(event.xbutton.state);
	  value ml_x = Val_int(event.xbutton.x);
	  value ml_y = Val_int(event.xbutton.y);
	  protect_callback4("button press callback",
			    &button_press_callback,
			    &ml_button, &ml_state, &ml_x, &ml_y);
	}
      break ;
    case ButtonRelease:
      if(event.xbutton.display == xdisplay &&
	 event.xbutton.window == xwindow &&
	 button_release_callback != default_callback)
	{
	  value ml_button = Val_int(event.xbutton.button - Button1);
	  value ml_state = Val_int(event.xbutton.state);
	  value ml_x = Val_int(event.xbutton.x);
	  value ml_y = Val_int(event.xbutton.y);
	  protect_callback4("button release callback",
			    &button_release_callback,
			    &ml_button, &ml_state, &ml_x, &ml_y);
	}
      break ;
    case MotionNotify:
      if(event.xmotion.display == xdisplay &&
	 event.xmotion.window == xwindow &&
	 motion_notify_callback != default_callback)
	{
	  value ml_state = Val_int(event.xmotion.state);
	  value ml_x = Val_int(event.xmotion.x);
	  value ml_y = Val_int(event.xmotion.y);

	  protect_callback3("motion notify callback", &motion_notify_callback,
			    &ml_state, &ml_x, &ml_y);
	}
      break ;
    case EnterNotify:
      if(event.xcrossing.display == xdisplay &&
	 event.xcrossing.window == xwindow &&
	 enter_window_callback != default_callback)
	{
	  value ml_state = Val_int(event.xcrossing.state);
	  value ml_x = Val_int(event.xcrossing.x);
	  value ml_y = Val_int(event.xcrossing.y);

	  protect_callback3("enter window callback", &enter_window_callback,
			    &ml_state, &ml_x, &ml_y);
	}
      break ;
    case LeaveNotify:
      if(event.xcrossing.display == xdisplay &&
	 event.xcrossing.window == xwindow &&
	 leave_window_callback != default_callback)
	{
	  value ml_state = Val_int(event.xcrossing.state);
	  value ml_x = Val_int(event.xcrossing.x);
	  value ml_y = Val_int(event.xcrossing.y);

	  protect_callback3("leave window callback", &leave_window_callback,
			    &ml_state, &ml_x, &ml_y);
	}
      break ;
    case FocusIn:
    case FocusOut:
      if(event.xfocus.display == xdisplay &&
	 event.xfocus.window == xwindow &&
	 leave_window_callback != default_callback)
	{
	  value ml_in = Val_bool(event.xfocus.type == FocusIn);

	  protect_callback("change focus callback", &leave_window_callback,
			    &ml_in );
	}
      break ;

    default: break ;
    }
  }
  main_loop_reentrant = 0 ;

  caml_acquire_runtime_system();
  CAMLreturn0 ;
}

void ml_egl_exit_loop()
{
  if(!main_loop_reentrant)
    failwith("Egl.exit_loop: can only be called inside main_loop\n") ;
  main_loop_continue = 0 ;
}

/****************************************************************************/
/*   MISCELLANEOUS                                                          */
/****************************************************************************/

void ml_egl_swap_buffers()
{
  if(!initialized) failwith("Egl.swap_buffers: not initialized") ;
  eglSwapBuffers(display, surface) ;
}

CAMLprim value ml_egl_query_version()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.query_version: not initialized") ;
  const char *s = eglQueryString(display, EGL_VERSION) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_vendor()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.query_vendor: not initialized") ;
  const char *s = eglQueryString(display, EGL_VENDOR) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_extensions()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.query_extensions: not initialized") ;
  const char *s = eglQueryString(display, EGL_EXTENSIONS) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_client_apis()
{
  CAMLparam0() ;
  if(!initialized)
    failwith("Egl.query_client_apis: not initialized") ;
  const char *s = eglQueryString(display, EGL_CLIENT_APIS) ;
  CAMLreturn(caml_copy_string(s));
}

CAMLprim value ml_egl_query_config()
{
  CAMLparam0() ;
  CAMLlocal1(ret) ;
  if(!initialized)
    failwith("Egl.query_config: not initialized") ;
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
