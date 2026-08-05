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
#include <X11/XKBlib.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/config.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/threads.h>
#include <caml/version.h>

#include "ml_egl.h"
#include "ml_egl_platform.h"


typedef struct platform_context_struct {
  Display *display;
  XIM xim;
  XIC xic;
} *platform_context ;

platform_context malloc_platform_context(egl_context ctxt) {
  platform_context res = (platform_context) malloc(sizeof(struct platform_context_struct));
  if (!res) {
    printf("Alloc platform_context failed\n"); fflush(stdout);
    exit(1);
  }
  return res;
}

/****************************************************************************/
/*   X IO ERROR HANDLING                                                    */
/****************************************************************************/

static int IOErrorHandler(Display *dpy)
{
  printf("IOErrorHandler \n"); fflush(stdout);
  exit(1);
}

void init_platform_ressources(egl_context ctxt, const char* name) {
  /* Open X Display */

  Display *display;

  if((display = XOpenDisplay(NULL)) == NULL)
    init_fail(ctxt, "cannot open X display") ;

  ctxt->platform->display = display;

  XIM xim = XOpenIM(display, NULL, NULL, NULL);

  if (!xim)
    init_fail(ctxt, "cannot create X input method") ;

  ctxt->platform->xim     = xim;

  ctxt->display = eglGetPlatformDisplay(EGL_PLATFORM_X11_KHR,
					(void *)display,
					NULL);
  /* Open X Window */
  ctxt->window = (EGLNativeWindowType)
    XCreateSimpleWindow(display,
			DefaultRootWindow(display),
			0, 0, ctxt->width, ctxt->height, 0, 0, 0) ;
  if(ctxt->window == None)
    init_fail(ctxt, "cannot create X window") ;

  XIC xic = XCreateIC(xim,
		 XNInputStyle,   XIMPreeditNothing | XIMStatusNothing,
		 XNClientWindow, ctxt->window,
		 XNFocusWindow,  ctxt->window,
		 NULL);

  if (!xic)
    init_fail(ctxt, "cannot create X input context") ;

  ctxt->platform->xic     = xic;

  XSelectInput(display, (Window) ctxt->window,
	       VisibilityChangeMask|
	       StructureNotifyMask|KeyPressMask|KeyReleaseMask|ExposureMask|
	       ButtonPressMask|ButtonReleaseMask|PointerMotionMask) ;
  XAutoRepeatOff(display);
  XFlush(display);
  XMapWindow(display, (Window) ctxt->window) ;
  XStoreName(display, (Window) ctxt->window, name) ;
  XSetIOErrorHandler(&IOErrorHandler) ;
}

void free_platform_ressources(egl_context ctxt) {
  if(ctxt->platform->display != NULL) {
    if (ctxt->window != None) {
      XDestroyWindow(ctxt->platform->display,
		     (Window) ctxt->window) ;
      ctxt->window = None ;
      XDestroyIC(ctxt->platform->xic) ;
      XCloseIM(ctxt->platform->xim) ;
      XCloseDisplay(ctxt->platform->display) ;
      ctxt->platform->display = NULL ;
      free(ctxt->platform);
    }
  }
}

egl_key x11_keysym_to_egl(KeySym ks)
{
  switch (ks)
  {
    /* ========================= */
    /* contrôle                 */
    /* ========================= */

    case XK_BackSpace: return EGL_KEY_Backspace;
    case XK_Tab:       return EGL_KEY_Tab;
    case XK_Linefeed:  return EGL_KEY_Linefeed;
    case XK_Clear:     return EGL_KEY_Clear;
    case XK_Return:    return EGL_KEY_Return;
    case XK_Pause:     return EGL_KEY_Pause;
    case XK_Scroll_Lock: return EGL_KEY_ScrollLock;
    case XK_Sys_Req:   return EGL_KEY_SysReq;
    case XK_Escape:    return EGL_KEY_Escape;
    case XK_Delete:    return EGL_KEY_Delete;

    /* ========================= */
    /* navigation               */
    /* ========================= */

    case XK_Home:      return EGL_KEY_Home;
    case XK_Left:      return EGL_KEY_Left;
    case XK_Up:        return EGL_KEY_Up;
    case XK_Right:     return EGL_KEY_Right;
    case XK_Down:      return EGL_KEY_Down;
    case XK_Prior:     return EGL_KEY_Prior;
    case XK_Next:      return EGL_KEY_Next;
    case XK_End:       return EGL_KEY_End;
    case XK_Begin:     return EGL_KEY_Begin;


    case XK_Select:    return EGL_KEY_Select;
    case XK_Print:     return EGL_KEY_Print;
    case XK_Execute:   return EGL_KEY_Execute;
    case XK_Insert:    return EGL_KEY_Insert;
    case XK_Undo:      return EGL_KEY_Undo;
    case XK_Redo:      return EGL_KEY_Redo;
    case XK_Menu:      return EGL_KEY_Menu;
    case XK_Find:      return EGL_KEY_Menu;
    case XK_Cancel:    return EGL_KEY_Menu;
    case XK_Help:      return EGL_KEY_Menu;
    case XK_Break:     return EGL_KEY_Menu;
    case XK_Mode_switch: return EGL_KEY_ModeSwitch;
    case XK_Num_Lock:   return EGL_KEY_NumLock;

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
    case XK_F25: return EGL_KEY_F25;
    case XK_F26: return EGL_KEY_F26;
    case XK_F27: return EGL_KEY_F27;
    case XK_F28: return EGL_KEY_F28;
    case XK_F29: return EGL_KEY_F29;
    case XK_F30: return EGL_KEY_F30;
    case XK_F31: return EGL_KEY_F31;
    case XK_F32: return EGL_KEY_F32;
    case XK_F33: return EGL_KEY_F33;
    case XK_F34: return EGL_KEY_F34;
    case XK_F35: return EGL_KEY_F35;

    /* ========================= */
    /* modifiers (touches)      */
    /* ========================= */

    case XK_Shift_L:    return EGL_KEY_ShiftLeft;
    case XK_Shift_R:    return EGL_KEY_ShiftRight;
    case XK_Control_L:  return EGL_KEY_ControlLeft;
    case XK_Control_R:  return EGL_KEY_ControlRight;
    case XK_Caps_Lock:  return EGL_KEY_CapsLock;
    case XK_Shift_Lock: return EGL_KEY_ShiftLock;

    case XK_Meta_L:     return EGL_KEY_MetaLeft;
    case XK_Meta_R:     return EGL_KEY_MetaRight;
    case XK_Alt_L:      return EGL_KEY_AltLeft;
    case XK_Alt_R:      return EGL_KEY_AltRight;
    case XK_Super_L:    return EGL_KEY_SuperLeft;
    case XK_Super_R:    return EGL_KEY_SuperRight;
    case XK_Hyper_L:    return EGL_KEY_HyperLeft;
    case XK_Hyper_R:    return EGL_KEY_HyperRight;


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

    case XK_KP_Space:    return EGL_KEY_KeypadSpace;
    case XK_KP_Tab:      return EGL_KEY_KeypadTab;
    case XK_KP_Enter:    return EGL_KEY_KeypadEnter;
    case XK_KP_F1:       return EGL_KEY_KeypadF1;
    case XK_KP_F2:       return EGL_KEY_KeypadF2;
    case XK_KP_F3:       return EGL_KEY_KeypadF3;
    case XK_KP_F4:       return EGL_KEY_KeypadF4;
    case XK_KP_Home:     return EGL_KEY_KeypadHome;
    case XK_KP_Left:     return EGL_KEY_KeypadLeft;
    case XK_KP_Up:       return EGL_KEY_KeypadUp;
    case XK_KP_Right:    return EGL_KEY_KeypadRight;
    case XK_KP_Down:     return EGL_KEY_KeypadDown;
    case XK_KP_Prior:    return EGL_KEY_KeypadPrior;
    case XK_KP_Next:     return EGL_KEY_KeypadNext;
    case XK_KP_End:      return EGL_KEY_KeypadEnd;
    case XK_KP_Begin:    return EGL_KEY_KeypadBegin;
    case XK_KP_Insert:   return EGL_KEY_KeypadInsert;
    case XK_KP_Delete:   return EGL_KEY_KeypadDelete;
    case XK_KP_Equal:    return EGL_KEY_KeypadEqual;
    case XK_KP_Multiply: return EGL_KEY_KeypadMultiply;
    case XK_KP_Add:      return EGL_KEY_KeypadAdd;
    case XK_KP_Separator:return EGL_KEY_KeypadSeparator;
    case XK_KP_Subtract: return EGL_KEY_KeypadSubtract;
    case XK_KP_Decimal:  return EGL_KEY_KeypadDecimal;
    case XK_KP_Divide:   return EGL_KEY_KeypadDivide;

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

CAMLprim value ml_egl_main_loop(value vc)
{
  CAMLparam1(vc) ;
  egl_context ctxt = Val_ctxt(vc);

  if(!ctxt->initialized)
    caml_failwith("Egl.main_loop: not initialized") ;

  int expected = 0 ;
  if(!atomic_compare_exchange_strong(&ctxt->main_loop_reentrant, &expected, 1))
    caml_failwith("Egl.main_loop: forbidden reentrant call") ;

  caml_release_runtime_system();

  XEvent event ;
  Display *display = (Display*) ctxt->platform->display;
  Window window = (Window) ctxt->window;
  Atom wmDeleteMessage = XInternAtom(display, "WM_DELETE_WINDOW", False);
  int window_visible = 0;
  XSetWMProtocols(display, window, &wmDeleteMessage, 1);

  ctxt->main_loop_continue = 1 ;

  while(ctxt->main_loop_continue) {
    if(ctxt->idle_callback != Val_unit && window_visible) {
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
	 ctxt->reshape_callback != Val_unit)
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
      if(event.xmap.display == display &&
	 event.xmap.window == window)
	window_visible = 0;
      break;
    case MapNotify:
      if(event.xmap.display == display &&
	 event.xmap.window == window)
	window_visible = 1;
      break;
    case VisibilityNotify:
      if(event.xvisibility.display == display &&
	 event.xvisibility.window == window)
	{
	  window_visible =
	    (event.xvisibility.state != VisibilityFullyObscured);
	  if (window_visible &&
	      ctxt->reshape_callback != Val_unit) {
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
	  if(ctxt->delete_callback == Val_unit)
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
	 ctxt->key_press_callback != Val_unit)
	{
	  char utf8[64];
	  KeySym keysym;
	  XKeyEvent ev = event.xkey;
	  ev.state &= ~(ControlMask | Mod1Mask | Mod4Mask);

	  int n = Xutf8LookupString(ctxt->platform->xic,
				    &ev,
				    utf8, sizeof(utf8), &keysym, NULL);
	  value ml_keysym = Val_int(0), ml_str = Val_int(0);
	  caml_acquire_runtime_system();
	  CAMLparam2(ml_keysym, ml_str);
	  if (n > 1 || (n == 1 && utf8[0] >= 32)) {
	    ml_str = caml_copy_string(utf8);
	    ml_keysym = caml_alloc_small(1, 0);  /* tag 0 = premier constructeur */
	    Field(ml_keysym, 0) = ml_str;
	  } else {
	    egl_key eglkey = x11_keysym_to_egl(keysym);
	    ml_keysym = Val_int(eglkey);
	  }
	  egl_mod eglmod = x11_state_to_egl(event.xkey.state);
	  value ml_state = Val_int(eglmod);
	  value ml_x = Val_int(event.xkey.x);
	  value ml_y = Val_int(event.xkey.y);
	  CAMLlocalN(tmp,4);
	  tmp[0] = ml_keysym; tmp[1]=ml_state; tmp[2]=ml_x; tmp[3]=ml_y ;
	  caml_callbackN_exn(ctxt->key_press_callback, 4, tmp);
	  CAMLdrop;
	  caml_release_runtime_system();
	}
      break ;
    case KeyRelease:
      if(event.xkey.display == display &&
	 event.xkey.window == window &&
	 ctxt->key_release_callback != Val_unit)
	{
	  char utf8[64];
	  KeySym keysym;
	  XKeyEvent ev = event.xkey;
	  ev.type = KeyPress;
	  ev.state &= ~(ControlMask | Mod1Mask | Mod4Mask);

	  int n = Xutf8LookupString(ctxt->platform->xic,
				    &ev,
				    utf8, sizeof(utf8), &keysym, NULL);
	  value ml_keysym = Val_int(0), ml_str = Val_int(0);
	  caml_acquire_runtime_system();
	  CAMLparam2(ml_keysym, ml_str);
	  if (n > 1 || (n == 1 && utf8[0] >= 32)) {
	    ml_str = caml_copy_string(utf8);
	    ml_keysym = caml_alloc_small(1, 0);  /* tag 0 = premier constructeur */
	    Field(ml_keysym, 0) = ml_str;
	  } else {
	    egl_key eglkey = x11_keysym_to_egl(keysym);
	    ml_keysym = Val_int(eglkey);
	  }
	  egl_mod eglmod = x11_state_to_egl(event.xkey.state);
	  value ml_state = Val_int(eglmod);
	  value ml_x = Val_int(event.xkey.x);
	  value ml_y = Val_int(event.xkey.y);
	  CAMLlocalN(tmp,4);
	  tmp[0] = ml_keysym; tmp[1]=ml_state; tmp[2]=ml_x; tmp[3]=ml_y ;
	  caml_callbackN_exn(ctxt->key_release_callback, 4, tmp);
	  CAMLdrop;
	  caml_release_runtime_system();
	}
      break ;
    case ButtonPress:
      if(event.xbutton.display == display &&
	 event.xbutton.window == window &&
	 ctxt->button_press_callback != Val_unit)
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
	 ctxt->button_release_callback != Val_unit)
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
	 ctxt->motion_notify_callback != Val_unit)
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
  caml_acquire_runtime_system();
  atomic_store(&ctxt->main_loop_reentrant, 0) ;
  CAMLreturn(Val_unit) ;
}
