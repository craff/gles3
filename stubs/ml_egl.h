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

extern void egl_connection_lost(void);
extern void init_fail(const char*);
extern EGLNativeDisplayType platform_display;
extern EGLNativeWindowType  platform_window;

extern void protect_callback(char *name, value *f, value *v1);
extern void protect_callback2(char *name, value *f, value *v1, value *v2);
extern void protect_callback3(char *name, value *f, value *v1,
			      value *v2, value *v3);
extern void protect_callback4(char *name, value *f, value *v1,
			value *v2, value *v3, value *v4);

extern int width;
extern int height;

extern int initialized;

extern int main_loop_reentrant;
extern int main_loop_continue;

extern value default_callback;
extern value idle_callback;
extern value reshape_callback;
extern value delete_callback;
extern value key_press_callback;
extern value key_release_callback;
extern value button_press_callback;
extern value button_release_callback;
extern value motion_notify_callback;


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
