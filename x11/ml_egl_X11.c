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
/* ml_gles_X11.c: Specific for X11 bachend                                  */
/****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <strings.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/XF86keysym.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <caml/version.h>
#include <uthash.h>

#include "ml_egl.h"
#include "ml_egl_platform.h"


typedef struct platform_context_struct { } *platform_context ;

platform_context malloc_platform_context(egl_context ctxt) {
  return NULL;
}

/****************************************************************************/
/*   X IO ERROR HANDLING                                                    */
/****************************************************************************/

typedef struct {
    Display *display;      // key
    egl_context context;         // your C object
    UT_hash_handle hh;
} display_entry;

static display_entry *displays = NULL;

static int IOErrorHandler(Display *dpy)
{
  printf("IOErrorHandler \n"); fflush(stdout);
  display_entry *e;

  HASH_FIND_PTR(displays, &dpy, e);

  /* If error came from another display, we call saved_IOErrorHandler */
  if(e) {
    /* X connection has been lost, we cannot free allocated resources.
       Instead, we try to restore an uninitialized state. */

    egl_context ctxt = e->context;
    egl_platform_lost(ctxt);
    ctxt->platform_window = None ;
    ctxt->platform_display = NULL ;
  }
  return 0 ;
}

void init_platform_ressources(egl_context ctxt, const char* name) {
  /* Open X Display */

  Display *display;

  if((display = XOpenDisplay(NULL)) == NULL)
    init_fail(ctxt, "cannot open X display") ;

  ctxt->platform_display = display;
  /* Open X Window */
  ctxt->platform_window =
    XCreateSimpleWindow(display,
			DefaultRootWindow(display),
			0, 0, ctxt->width, ctxt->height, 0, 0, 0) ;
  if(ctxt->platform_window == None)
    init_fail(ctxt, "cannot create X window") ;
  XSelectInput(display, ctxt->platform_window,
	       VisibilityChangeMask|
	       StructureNotifyMask|KeyPressMask|KeyReleaseMask|ExposureMask|
	       ButtonPressMask|ButtonReleaseMask|PointerMotionMask) ;
  XMapWindow(display, ctxt->platform_window) ;
  XStoreName(display, ctxt->platform_window, name) ;
  display_entry *e = malloc(sizeof(*e));

  e->display = display;
  e->context = ctxt;

  HASH_ADD_PTR(displays, display, e);
  XSetIOErrorHandler(&IOErrorHandler) ;
}

void free_platform_ressources(egl_context ctxt) {
  display_entry *e;
  HASH_FIND_PTR(displays, &(ctxt->platform_display), e);

  if (e) {
    HASH_DEL(displays, e);
    free(e);
  }
  if(ctxt->platform_display != NULL) {
    if (ctxt->platform_window != None) {
      XDestroyWindow(ctxt->platform_display, ctxt->platform_window) ;
      ctxt->platform_window = None ;
      XCloseDisplay(ctxt->platform_display) ;
      ctxt->platform_display = NULL ;
    }
  }
}

egl_key x11_keysym_to_egl(KeySym ks)
{
  switch (ks)
  {
    /* ========================= */
    /* lettres (lower/upper safe) */
    /* ========================= */

    case XK_A: case XK_a: return EGL_KEY_A;
    case XK_B: case XK_b: return EGL_KEY_B;
    case XK_C: case XK_c: return EGL_KEY_C;
    case XK_D: case XK_d: return EGL_KEY_D;
    case XK_E: case XK_e: return EGL_KEY_E;
    case XK_F: case XK_f: return EGL_KEY_F;
    case XK_G: case XK_g: return EGL_KEY_G;
    case XK_H: case XK_h: return EGL_KEY_H;
    case XK_I: case XK_i: return EGL_KEY_I;
    case XK_J: case XK_j: return EGL_KEY_J;
    case XK_K: case XK_k: return EGL_KEY_K;
    case XK_L: case XK_l: return EGL_KEY_L;
    case XK_M: case XK_m: return EGL_KEY_M;
    case XK_N: case XK_n: return EGL_KEY_N;
    case XK_O: case XK_o: return EGL_KEY_O;
    case XK_P: case XK_p: return EGL_KEY_P;
    case XK_Q: case XK_q: return EGL_KEY_Q;
    case XK_R: case XK_r: return EGL_KEY_R;
    case XK_S: case XK_s: return EGL_KEY_S;
    case XK_T: case XK_t: return EGL_KEY_T;
    case XK_U: case XK_u: return EGL_KEY_U;
    case XK_V: case XK_v: return EGL_KEY_V;
    case XK_W: case XK_w: return EGL_KEY_W;
    case XK_X: case XK_x: return EGL_KEY_X;
    case XK_Y: case XK_y: return EGL_KEY_Y;
    case XK_Z: case XK_z: return EGL_KEY_Z;

    /* ========================= */
    /* chiffres (haut clavier)   */
    /* ========================= */

    case XK_0: return EGL_KEY_Num0;
    case XK_1: return EGL_KEY_Num1;
    case XK_2: return EGL_KEY_Num2;
    case XK_3: return EGL_KEY_Num3;
    case XK_4: return EGL_KEY_Num4;
    case XK_5: return EGL_KEY_Num5;
    case XK_6: return EGL_KEY_Num6;
    case XK_7: return EGL_KEY_Num7;
    case XK_8: return EGL_KEY_Num8;
    case XK_9: return EGL_KEY_Num9;

    /* ========================= */
    /* contrôle                 */
    /* ========================= */

    case XK_Escape:   return EGL_KEY_Escape;
    case XK_Return:    return EGL_KEY_Return;
    case XK_Tab:       return EGL_KEY_Tab;
    case XK_BackSpace: return EGL_KEY_Backspace;
    case XK_Delete:    return EGL_KEY_Delete;
    case XK_Insert:    return EGL_KEY_Insert;

    /* ========================= */
    /* espace / ponctuation     */
    /* ========================= */

    case XK_space:      return EGL_KEY_Space;
    case XK_minus:      return EGL_KEY_Minus;
    case XK_equal:      return EGL_KEY_Equal;

    case XK_bracketleft:  return EGL_KEY_LeftBracket;
    case XK_bracketright: return EGL_KEY_RightBracket;

    case XK_backslash:  return EGL_KEY_Backslash;

    case XK_semicolon:  return EGL_KEY_Semicolon;
    case XK_apostrophe: return EGL_KEY_Apostrophe;

    case XK_comma:      return EGL_KEY_Comma;
    case XK_period:     return EGL_KEY_Period;
    case XK_slash:      return EGL_KEY_Slash;

    case XK_grave:      return EGL_KEY_Grave;

    /* ========================= */
    /* système                  */
    /* ========================= */

    case XK_Print:     return EGL_KEY_PrintScreen;
    case XK_Scroll_Lock: return EGL_KEY_ScrollLock;
    case XK_Pause:     return EGL_KEY_Pause;
    case XK_Menu:      return EGL_KEY_Menu;

    /* ========================= */
    /* navigation               */
    /* ========================= */

    case XK_Home:      return EGL_KEY_Home;
    case XK_End:       return EGL_KEY_End;
    case XK_Page_Up:   return EGL_KEY_PageUp;
    case XK_Page_Down: return EGL_KEY_PageDown;

    case XK_Left:      return EGL_KEY_Left;
    case XK_Right:     return EGL_KEY_Right;
    case XK_Up:        return EGL_KEY_Up;
    case XK_Down:      return EGL_KEY_Down;

    /* ========================= */
    /* fonctions F1–F24        */
    /* ========================= */

    case XK_F1:  return EGL_KEY_F1;
    case XK_F2:  return EGL_KEY_F2;
    case XK_F3:  return EGL_KEY_F3;
    case XK_F4:  return EGL_KEY_F4;
    case XK_F5:  return EGL_KEY_F5;
    case XK_F6:  return EGL_KEY_F6;
    case XK_F7:  return EGL_KEY_F7;
    case XK_F8:  return EGL_KEY_F8;
    case XK_F9:  return EGL_KEY_F9;
    case XK_F10: return EGL_KEY_F10;
    case XK_F11: return EGL_KEY_F11;
    case XK_F12: return EGL_KEY_F12;

    case XK_F13: return EGL_KEY_F13;
    case XK_F14: return EGL_KEY_F14;
    case XK_F15: return EGL_KEY_F15;
    case XK_F16: return EGL_KEY_F16;
    case XK_F17: return EGL_KEY_F17;
    case XK_F18: return EGL_KEY_F18;
    case XK_F19: return EGL_KEY_F19;
    case XK_F20: return EGL_KEY_F20;
    case XK_F21: return EGL_KEY_F21;
    case XK_F22: return EGL_KEY_F22;
    case XK_F23: return EGL_KEY_F23;
    case XK_F24: return EGL_KEY_F24;

    /* ========================= */
    /* modifiers (touches)      */
    /* ========================= */

    case XK_Shift_L:   return EGL_KEY_ShiftLeft;
    case XK_Shift_R:   return EGL_KEY_ShiftRight;

    case XK_Control_L:  return EGL_KEY_ControlLeft;
    case XK_Control_R:  return EGL_KEY_ControlRight;

    case XK_Alt_L:      return EGL_KEY_AltLeft;
    case XK_Alt_R:      return EGL_KEY_AltRight;

    case XK_Super_L:    return EGL_KEY_SuperLeft;
    case XK_Super_R:    return EGL_KEY_SuperRight;

    case XK_Caps_Lock:  return EGL_KEY_CapsLock;
    case XK_Num_Lock:   return EGL_KEY_NumLock;

    /* ========================= */
    /* keypad                   */
    /* ========================= */

    case XK_KP_0: return EGL_KEY_Keypad0;
    case XK_KP_1: return EGL_KEY_Keypad1;
    case XK_KP_2: return EGL_KEY_Keypad2;
    case XK_KP_3: return EGL_KEY_Keypad3;
    case XK_KP_4: return EGL_KEY_Keypad4;
    case XK_KP_5: return EGL_KEY_Keypad5;
    case XK_KP_6: return EGL_KEY_Keypad6;
    case XK_KP_7: return EGL_KEY_Keypad7;
    case XK_KP_8: return EGL_KEY_Keypad8;
    case XK_KP_9: return EGL_KEY_Keypad9;

    case XK_KP_Decimal:  return EGL_KEY_KeypadDecimal;
    case XK_KP_Divide:   return EGL_KEY_KeypadDivide;
    case XK_KP_Multiply: return EGL_KEY_KeypadMultiply;
    case XK_KP_Subtract: return EGL_KEY_KeypadSubtract;
    case XK_KP_Add:      return EGL_KEY_KeypadAdd;
    case XK_KP_Enter:    return EGL_KEY_KeypadEnter;
    case XK_KP_Equal:    return EGL_KEY_KeypadEqual;

    /* ========================= */
    /* multimedia               */
    /* ========================= */

    case XF86XK_AudioRaiseVolume: return EGL_KEY_VolumeUp;
    case XF86XK_AudioLowerVolume: return EGL_KEY_VolumeDown;
    case XF86XK_AudioMute:        return EGL_KEY_VolumeMute;

    case XF86XK_AudioPlay:        return EGL_KEY_MediaPlay;
    case XF86XK_AudioStop:        return EGL_KEY_MediaStop;
    case XF86XK_AudioPrev:        return EGL_KEY_MediaPrevious;
    case XF86XK_AudioNext:        return EGL_KEY_MediaNext;

    case XF86XK_MonBrightnessUp:   return EGL_KEY_BrightnessUp;
    case XF86XK_MonBrightnessDown: return EGL_KEY_BrightnessDown;

    case XF86XK_Eject:             return EGL_KEY_Eject;

    /* ========================= */
    /* fallback                 */
    /* ========================= */

    default:
      return EGL_KEY_Unknown;
  }
}

egl_button x11_button_to_egl(unsigned int button)
{
  switch (button)
  {
    case Button1:
      return EGL_BUTTON_Left;

    case Button2:
      return EGL_BUTTON_Middle;

    case Button3:
      return EGL_BUTTON_Right;

    case Button4:
      return EGL_BUTTON_ScrollUp;

    case Button5:
      return EGL_BUTTON_ScrollDown;

    /* scroll horizontal (selon drivers) */
    case 6:
      return EGL_BUTTON_ScrollLeft;

    case 7:
      return EGL_BUTTON_ScrollRight;

    /* boutons supplémentaires souris gaming */
    case 8:
      return EGL_BUTTON_Button4;

    case 9:
      return EGL_BUTTON_Button5;

    case 10:
      return EGL_BUTTON_Button6;

    case 11:
      return EGL_BUTTON_Button7;

    case 12:
      return EGL_BUTTON_Button8;

    case 13:
      return EGL_BUTTON_Button9;

    case 14:
      return EGL_BUTTON_Button10;

    default:
      return EGL_BUTTON_Unknown;
  }
}

egl_mod x11_state_to_egl(unsigned int state)
{
  int m = 0;

  if (state & ShiftMask)
    m |= EGL_MOD_Shift;

  if (state & ControlMask)
    m |= EGL_MOD_Control;

  if (state & Mod1Mask)   // Alt (le plus courant)
    m |= EGL_MOD_Alt;

  if (state & Mod4Mask)   // Super / Windows key
    m |= EGL_MOD_Super;

  if (state & LockMask)   // CapsLock
    m |= EGL_MOD_CapsLock;

  if (state & Mod2Mask)   // NumLock (très souvent)
    m |= EGL_MOD_NumLock;

  return (egl_mod)m;
}

/****************************************************************************/
/*   MAIN LOOP                                                              */
/****************************************************************************/

CAMLprim value ml_egl_main_loop(egl_context ctxt)
{
  CAMLparam0() ;
  XEvent event ;
  int window_visible = 0;

  if(!ctxt->initialized)
    caml_failwith("Egl.main_loop: not initialized") ;

  if(ctxt->main_loop_reentrant)
    caml_failwith("Egl.main_loop: forbidden reentrant call") ;

  caml_release_runtime_system();

  ctxt->main_loop_reentrant = 1 ;

  Display *display = ctxt->platform_display;
  Window window = ctxt->platform_window;
  Atom wmDeleteMessage = XInternAtom(display, "WM_DELETE_WINDOW", False);
  XSetWMProtocols(display, window, &wmDeleteMessage, 1);

  ctxt->main_loop_continue = 1 ;

  while(ctxt->main_loop_continue) {

    if(ctxt->idle_callback != ctxt->default_callback && window_visible) {
      while(XPending(display) == 0) {
	value u = Val_unit;
	protect_callback("idle callback", &(ctxt->idle_callback), &u) ;
      }
    }
    XNextEvent(display, &event) ;

    switch(event.type) {
    case ConfigureNotify:
      if(event.xconfigure.display == display &&
	 event.xconfigure.window == window &&
	 (event.xconfigure.width != ctxt->width ||
	  event.xconfigure.height != ctxt->height) &&
	 ctxt->reshape_callback != ctxt->default_callback)
	{
	  ctxt->width = event.xconfigure.width ;
	  ctxt->height = event.xconfigure.height ;
	  value ml_width = Val_int(ctxt->width);
	  value ml_height = Val_int(ctxt->height);
	  protect_callback2("reshape callback",
			    &(ctxt->reshape_callback),
			    &ml_width, &ml_height) ;
	}
      break ;
    case UnmapNotify:
      printf("unmap\n");
      if(event.xmap.display == display &&
	 event.xmap.window == window)
	window_visible = 0;
      break;
    case MapNotify:
      printf("map\n");
      if(event.xmap.display == display &&
	 event.xmap.window == window)
	window_visible = 1;
      break;
    case VisibilityNotify:
      printf("avant visible: %d\n", window_visible);
      if(event.xvisibility.display == display &&
	 event.xvisibility.window == window)
	{
	  window_visible =
	    (event.xvisibility.state != VisibilityFullyObscured);
	  printf("visible: %d\n", window_visible);
	  if (window_visible &&
	      ctxt->reshape_callback != ctxt->default_callback) {
	    value ml_width = Val_int(ctxt->width);
	    value ml_height = Val_int(ctxt->height);
	    protect_callback2("reshape callback",
			      &(ctxt->reshape_callback),
			      &ml_width, &ml_height) ;
	  }
	}
      break ;
    case ClientMessage:
      if(event.xclient.display == display &&
	 event.xclient.window == window &&
	 event.xclient.data.l[0] == wmDeleteMessage)
	{
	  if(ctxt->delete_callback == ctxt->default_callback)
	    ctxt->main_loop_continue = 0 ;
	  else
	    {
	      value u = Val_unit;
	      protect_callback("delete callback", &(ctxt->delete_callback), &u) ;
	    }
	}
      break ;
    case KeyPress:
      if(event.xkey.display == display &&
	 event.xkey.window == window &&
	 ctxt->key_press_callback != ctxt->default_callback)
	{
	  KeySym keysym ;
	  XLookupString(&(event.xkey), NULL, 0, &keysym, NULL) ;
	  egl_key eglkey = x11_keysym_to_egl(keysym);
	  egl_mod eglmod = x11_state_to_egl(event.xkey.state);

	  value ml_keysym = Val_int(eglkey);
	  value ml_state = Val_int(eglmod);
	  value ml_x = Val_int(event.xkey.x);
	  value ml_y = Val_int(event.xkey.y);
	  protect_callback4("key press callback", &(ctxt->key_press_callback),
			    &ml_keysym, &ml_state, &ml_x, &ml_y);
	}
      break ;
    case KeyRelease:
      if(event.xkey.display == display &&
	 event.xkey.window == window &&
	 ctxt->key_release_callback != ctxt->default_callback)
	{
	  KeySym keysym ;
	  XLookupString(&(event.xkey), NULL, 0, &keysym, NULL) ;
	  egl_key eglkey = x11_keysym_to_egl(keysym);
	  egl_mod eglmod = x11_state_to_egl(event.xkey.state);

	  value ml_keysym = Val_int(eglkey);
	  value ml_state = Val_int(eglmod);
	  value ml_x = Val_int(event.xkey.x);
	  value ml_y = Val_int(event.xkey.y);
	  protect_callback4("key release callback", &(ctxt->key_release_callback),
			    &ml_keysym, &ml_state, &ml_x, &ml_y);
	}
      break ;
    case ButtonPress:
      if(event.xbutton.display == display &&
	 event.xbutton.window == window &&
	 ctxt->button_press_callback != ctxt->default_callback)
	{
	  egl_button eglbut = x11_button_to_egl(event.xbutton.button);
	  egl_mod eglmod = x11_state_to_egl(event.xkey.state);

	  value ml_button = Val_int(eglbut);
	  value ml_state = Val_int(eglmod);
	  value ml_x = Val_int(event.xbutton.x);
	  value ml_y = Val_int(event.xbutton.y);
	  protect_callback4("button press callback",
			    &(ctxt->button_press_callback),
			    &ml_button, &ml_state, &ml_x, &ml_y);
	}
      break ;
    case ButtonRelease:
      if(event.xbutton.display == display &&
	 event.xbutton.window == window &&
	 ctxt->button_release_callback != ctxt->default_callback)
	{
	  egl_button eglbut = x11_button_to_egl(event.xbutton.button);
	  egl_mod eglmod = x11_state_to_egl(event.xkey.state);

	  value ml_button = Val_int(eglbut);
	  value ml_state = Val_int(eglmod);
	  value ml_x = Val_int(event.xbutton.x);
	  value ml_y = Val_int(event.xbutton.y);
	  protect_callback4("button release callback",
			    &(ctxt->button_release_callback),
			    &ml_button, &ml_state, &ml_x, &ml_y);
	}
      break ;
    case MotionNotify:
      if(event.xmotion.display == display &&
	 event.xmotion.window == window &&
	 ctxt->motion_notify_callback != ctxt->default_callback)
	{
	  value ml_state = Val_int(event.xkey.state);
	  value ml_x = Val_int(event.xmotion.x);
	  value ml_y = Val_int(event.xmotion.y);

	  protect_callback3("motion notify callback",
			    &(ctxt->motion_notify_callback),
			    &ml_state, &ml_x, &ml_y);
	}
      break ;
    default: break ;
    }
  }
  ctxt->main_loop_reentrant = 0 ;

  caml_acquire_runtime_system();
  CAMLreturn(Val_unit) ;
}
