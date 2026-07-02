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
#ifndef ML_EGL_H
#define ML_EGL_H

#include <EGL/egl.h>
#include <caml/memory.h>
#include <stdatomic.h>

typedef struct platform_context_struct *platform_context;

typedef struct egl_context_struct {
  EGLNativeDisplayType platform_display;
  EGLNativeWindowType  platform_window;
  EGLDisplay display;
  EGLConfig config ;
  EGLSurface surface;
  EGLContext context;
  int width;
  int height;
  int initialized;
  atomic_int main_loop_reentrant;
  int main_loop_continue;
  value idle_callback;
  value reshape_callback;
  value delete_callback;
  value key_press_callback;
  value key_release_callback;
  value button_press_callback;
  value button_release_callback;
  value motion_notify_callback;
  platform_context platform;
} *egl_context;

#define Val_ctxt(v) (*((egl_context *) Data_custom_val(v)))

extern void egl_connection_lost(egl_context);
extern void init_fail(egl_context, const char*);


extern void protect_callback(char *name, value *f, value *v1);
extern void protect_callback2(char *name, value *f, value *v1, value *v2);
extern void protect_callback3(char *name, value *f, value *v1,
			      value *v2, value *v3);
extern void protect_callback4(char *name, value *f, value *v1,
			value *v2, value *v3, value *v4);


typedef enum {
#define KEY(x) EGL_KEY_##x,
#include "key.h"
#undef KEY

    EGL_KEY_COUNT
} egl_key;

typedef enum {
#define BUTTON(x) EGL_BUTTON_##x,
#include "button.h"
#undef BUTTON

    EGL_BUTTON_COUNT
} egl_button;

typedef int egl_mod;

#define MODIFIER(x,code) extern egl_mod EGL_MOD_##x;
#include "modifier.h"
#undef MODIFIER



#endif
