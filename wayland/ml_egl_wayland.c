/****************************************************************************/
/* EGL + Wayland backend (single file)                                      */
/****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <linux/input-event-codes.h>

#include <wayland-client.h>
#include <wayland-egl.h>
#include <EGL/egl.h>
#include <wayland-client-protocol.h>
#include "xdg-shell-client-protocol.h"

#include <xkbcommon/xkbcommon.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/fail.h>
#include <caml/threads.h>

#include "ml_egl.h"
#include "ml_egl_platform.h"

/* ========================= */
/* Platform globals          */
/* ========================= */

static struct wl_registry   *wl_registry = NULL;
static struct wl_compositor *wl_compositor = NULL;
static struct wl_surface    *wl_surface = NULL;

static struct wl_seat      *wl_seat = NULL;
static struct wl_keyboard  *wl_keyboard = NULL;
static struct wl_pointer   *wl_pointer = NULL;

static struct xdg_wm_base *xdg_wm_base = NULL;
static struct xdg_surface *xdg_surface = NULL;
static struct xdg_toplevel *xdg_toplevel = NULL;

/* ========================= */
/* XKB                      */
/* ========================= */

static struct xkb_context *xkb_ctx = NULL;
static struct xkb_keymap   *xkb_keymap = NULL;
static struct xkb_state    *xkb_state = NULL;
static int   mouse_x = 0;
static int   mouse_y = 0;
static int   window_visible = 0;

/* ========================= */
/* Registry                 */
/* ========================= */

static void registry_global(void *data,
                            struct wl_registry *registry,
                            uint32_t name,
                            const char *interface,
                            uint32_t version)
{
  (void)data;

  if (strcmp(interface, "wl_compositor") == 0)
    wl_compositor = wl_registry_bind(registry, name,
                                     &wl_compositor_interface, 4);

  if (strcmp(interface, "wl_seat") == 0)
    wl_seat = wl_registry_bind(registry, name,
                               &wl_seat_interface, 7);

  if (strcmp(interface, "xdg_wm_base") == 0)
      xdg_wm_base = wl_registry_bind(registry, name,
				     &xdg_wm_base_interface, 1);
}

static void registry_remove(void *data,
                            struct wl_registry *registry,
                            uint32_t name)
{
  (void)data; (void)registry; (void)name;
}

static const struct wl_registry_listener registry_listener =
{
  registry_global,
  registry_remove
};

/* ========================= */
/* Key mapping              */
/* ========================= */

static egl_key xkb_to_egl(xkb_keysym_t sym)
{
  switch(sym)
  {
    /* ========================= */
    /* lettres (lower/upper safe) */
    /* ========================= */

    case XKB_KEY_A: case XKB_KEY_a: return EGL_KEY_A;
    case XKB_KEY_B: case XKB_KEY_b: return EGL_KEY_B;
    case XKB_KEY_C: case XKB_KEY_c: return EGL_KEY_C;
    case XKB_KEY_D: case XKB_KEY_d: return EGL_KEY_D;
    case XKB_KEY_E: case XKB_KEY_e: return EGL_KEY_E;
    case XKB_KEY_F: case XKB_KEY_f: return EGL_KEY_F;
    case XKB_KEY_G: case XKB_KEY_g: return EGL_KEY_G;
    case XKB_KEY_H: case XKB_KEY_h: return EGL_KEY_H;
    case XKB_KEY_I: case XKB_KEY_i: return EGL_KEY_I;
    case XKB_KEY_J: case XKB_KEY_j: return EGL_KEY_J;
    case XKB_KEY_K: case XKB_KEY_k: return EGL_KEY_K;
    case XKB_KEY_L: case XKB_KEY_l: return EGL_KEY_L;
    case XKB_KEY_M: case XKB_KEY_m: return EGL_KEY_M;
    case XKB_KEY_N: case XKB_KEY_n: return EGL_KEY_N;
    case XKB_KEY_O: case XKB_KEY_o: return EGL_KEY_O;
    case XKB_KEY_P: case XKB_KEY_p: return EGL_KEY_P;
    case XKB_KEY_Q: case XKB_KEY_q: return EGL_KEY_Q;
    case XKB_KEY_R: case XKB_KEY_r: return EGL_KEY_R;
    case XKB_KEY_S: case XKB_KEY_s: return EGL_KEY_S;
    case XKB_KEY_T: case XKB_KEY_t: return EGL_KEY_T;
    case XKB_KEY_U: case XKB_KEY_u: return EGL_KEY_U;
    case XKB_KEY_V: case XKB_KEY_v: return EGL_KEY_V;
    case XKB_KEY_W: case XKB_KEY_w: return EGL_KEY_W;
    case XKB_KEY_X: case XKB_KEY_x: return EGL_KEY_X;
    case XKB_KEY_Y: case XKB_KEY_y: return EGL_KEY_Y;
    case XKB_KEY_Z: case XKB_KEY_z: return EGL_KEY_Z;

    /* ========================= */
    /* chiffres (haut clavier)   */
    /* ========================= */

    case XKB_KEY_0: return EGL_KEY_Num0;
    case XKB_KEY_1: return EGL_KEY_Num1;
    case XKB_KEY_2: return EGL_KEY_Num2;
    case XKB_KEY_3: return EGL_KEY_Num3;
    case XKB_KEY_4: return EGL_KEY_Num4;
    case XKB_KEY_5: return EGL_KEY_Num5;
    case XKB_KEY_6: return EGL_KEY_Num6;
    case XKB_KEY_7: return EGL_KEY_Num7;
    case XKB_KEY_8: return EGL_KEY_Num8;
    case XKB_KEY_9: return EGL_KEY_Num9;

    /* ========================= */
    /* contrôle                 */
    /* ========================= */

    case XKB_KEY_Escape:   return EGL_KEY_Escape;
    case XKB_KEY_Return:    return EGL_KEY_Return;
    case XKB_KEY_Tab:       return EGL_KEY_Tab;
    case XKB_KEY_BackSpace: return EGL_KEY_Backspace;
    case XKB_KEY_Delete:    return EGL_KEY_Delete;
    case XKB_KEY_Insert:    return EGL_KEY_Insert;

    /* ========================= */
    /* espace / ponctuation     */
    /* ========================= */

    case XKB_KEY_space:      return EGL_KEY_Space;
    case XKB_KEY_minus:      return EGL_KEY_Minus;
    case XKB_KEY_equal:      return EGL_KEY_Equal;

    case XKB_KEY_bracketleft:  return EGL_KEY_LeftBracket;
    case XKB_KEY_bracketright: return EGL_KEY_RightBracket;

    case XKB_KEY_backslash:  return EGL_KEY_Backslash;

    case XKB_KEY_semicolon:  return EGL_KEY_Semicolon;
    case XKB_KEY_apostrophe: return EGL_KEY_Apostrophe;

    case XKB_KEY_comma:      return EGL_KEY_Comma;
    case XKB_KEY_period:     return EGL_KEY_Period;
    case XKB_KEY_slash:      return EGL_KEY_Slash;

    case XKB_KEY_grave:      return EGL_KEY_Grave;

    /* ========================= */
    /* système                  */
    /* ========================= */

    case XKB_KEY_Print:     return EGL_KEY_PrintScreen;
    case XKB_KEY_Scroll_Lock: return EGL_KEY_ScrollLock;
    case XKB_KEY_Pause:     return EGL_KEY_Pause;
    case XKB_KEY_Menu:      return EGL_KEY_Menu;

    /* ========================= */
    /* navigation               */
    /* ========================= */

    case XKB_KEY_Home:      return EGL_KEY_Home;
    case XKB_KEY_End:       return EGL_KEY_End;
    case XKB_KEY_Page_Up:   return EGL_KEY_PageUp;
    case XKB_KEY_Page_Down: return EGL_KEY_PageDown;

    case XKB_KEY_Left:      return EGL_KEY_Left;
    case XKB_KEY_Right:     return EGL_KEY_Right;
    case XKB_KEY_Up:        return EGL_KEY_Up;
    case XKB_KEY_Down:      return EGL_KEY_Down;

    /* ========================= */
    /* fonctions F1–F24        */
    /* ========================= */

    case XKB_KEY_F1:  return EGL_KEY_F1;
    case XKB_KEY_F2:  return EGL_KEY_F2;
    case XKB_KEY_F3:  return EGL_KEY_F3;
    case XKB_KEY_F4:  return EGL_KEY_F4;
    case XKB_KEY_F5:  return EGL_KEY_F5;
    case XKB_KEY_F6:  return EGL_KEY_F6;
    case XKB_KEY_F7:  return EGL_KEY_F7;
    case XKB_KEY_F8:  return EGL_KEY_F8;
    case XKB_KEY_F9:  return EGL_KEY_F9;
    case XKB_KEY_F10: return EGL_KEY_F10;
    case XKB_KEY_F11: return EGL_KEY_F11;
    case XKB_KEY_F12: return EGL_KEY_F12;

    case XKB_KEY_F13: return EGL_KEY_F13;
    case XKB_KEY_F14: return EGL_KEY_F14;
    case XKB_KEY_F15: return EGL_KEY_F15;
    case XKB_KEY_F16: return EGL_KEY_F16;
    case XKB_KEY_F17: return EGL_KEY_F17;
    case XKB_KEY_F18: return EGL_KEY_F18;
    case XKB_KEY_F19: return EGL_KEY_F19;
    case XKB_KEY_F20: return EGL_KEY_F20;
    case XKB_KEY_F21: return EGL_KEY_F21;
    case XKB_KEY_F22: return EGL_KEY_F22;
    case XKB_KEY_F23: return EGL_KEY_F23;
    case XKB_KEY_F24: return EGL_KEY_F24;

    /* ========================= */
    /* modifiers (touches)      */
    /* ========================= */

    case XKB_KEY_Shift_L:   return EGL_KEY_ShiftLeft;
    case XKB_KEY_Shift_R:   return EGL_KEY_ShiftRight;

    case XKB_KEY_Control_L:  return EGL_KEY_ControlLeft;
    case XKB_KEY_Control_R:  return EGL_KEY_ControlRight;

    case XKB_KEY_Alt_L:      return EGL_KEY_AltLeft;
    case XKB_KEY_Alt_R:      return EGL_KEY_AltRight;

    case XKB_KEY_Super_L:    return EGL_KEY_SuperLeft;
    case XKB_KEY_Super_R:    return EGL_KEY_SuperRight;

    case XKB_KEY_Caps_Lock:  return EGL_KEY_CapsLock;
    case XKB_KEY_Num_Lock:   return EGL_KEY_NumLock;

    /* ========================= */
    /* keypad                   */
    /* ========================= */

    case XKB_KEY_KP_0: return EGL_KEY_Keypad0;
    case XKB_KEY_KP_1: return EGL_KEY_Keypad1;
    case XKB_KEY_KP_2: return EGL_KEY_Keypad2;
    case XKB_KEY_KP_3: return EGL_KEY_Keypad3;
    case XKB_KEY_KP_4: return EGL_KEY_Keypad4;
    case XKB_KEY_KP_5: return EGL_KEY_Keypad5;
    case XKB_KEY_KP_6: return EGL_KEY_Keypad6;
    case XKB_KEY_KP_7: return EGL_KEY_Keypad7;
    case XKB_KEY_KP_8: return EGL_KEY_Keypad8;
    case XKB_KEY_KP_9: return EGL_KEY_Keypad9;

    case XKB_KEY_KP_Decimal:  return EGL_KEY_KeypadDecimal;
    case XKB_KEY_KP_Divide:   return EGL_KEY_KeypadDivide;
    case XKB_KEY_KP_Multiply: return EGL_KEY_KeypadMultiply;
    case XKB_KEY_KP_Subtract: return EGL_KEY_KeypadSubtract;
    case XKB_KEY_KP_Add:      return EGL_KEY_KeypadAdd;
    case XKB_KEY_KP_Enter:    return EGL_KEY_KeypadEnter;
    case XKB_KEY_KP_Equal:    return EGL_KEY_KeypadEqual;

    /* ========================= */
    /* multimedia               */
    /* ========================= */

    case XKB_KEY_XF86AudioRaiseVolume: return EGL_KEY_VolumeUp;
    case XKB_KEY_XF86AudioLowerVolume: return EGL_KEY_VolumeDown;
    case XKB_KEY_XF86AudioMute:        return EGL_KEY_VolumeMute;

    case XKB_KEY_XF86AudioPlay:        return EGL_KEY_MediaPlay;
    case XKB_KEY_XF86AudioStop:        return EGL_KEY_MediaStop;
    case XKB_KEY_XF86AudioPrev:        return EGL_KEY_MediaPrevious;
    case XKB_KEY_XF86AudioNext:        return EGL_KEY_MediaNext;

    case XKB_KEY_XF86MonBrightnessUp:   return EGL_KEY_BrightnessUp;
    case XKB_KEY_XF86MonBrightnessDown: return EGL_KEY_BrightnessDown;

    case XKB_KEY_XF86Eject:             return EGL_KEY_Eject;

    /* ========================= */
    /* fallback                 */
    /* ========================= */

    default:
      return EGL_KEY_Unknown;
  }
}

static egl_mod xkb_mod_to_egl(struct xkb_state *state)
{
  egl_mod m = 0;

  if (xkb_state_mod_name_is_active(state, XKB_MOD_NAME_SHIFT, XKB_STATE_MODS_EFFECTIVE))
    m |= EGL_MOD_Shift;

  if (xkb_state_mod_name_is_active(state, XKB_MOD_NAME_CTRL, XKB_STATE_MODS_EFFECTIVE))
    m |= EGL_MOD_Control;

  if (xkb_state_mod_name_is_active(state, XKB_MOD_NAME_ALT, XKB_STATE_MODS_EFFECTIVE))
    m |= EGL_MOD_Alt;

  if (xkb_state_mod_name_is_active(state, XKB_MOD_NAME_LOGO, XKB_STATE_MODS_EFFECTIVE))
    m |= EGL_MOD_Super;

  return m;
}

/* ========================= */
/* Keyboard                 */
/* ========================= */

static void keyboard_keymap(void *data,
                             struct wl_keyboard *kbd,
                             uint32_t format,
                             int fd,
                             uint32_t size)
{
  (void)data; (void)kbd;

  char *map = mmap(NULL, size, PROT_READ, MAP_SHARED, fd, 0);

  xkb_keymap = xkb_keymap_new_from_string(
      xkb_ctx,
      map,
      XKB_KEYMAP_FORMAT_TEXT_V1,
      XKB_KEYMAP_COMPILE_NO_FLAGS);

  munmap(map, size);
  close(fd);

  xkb_state = xkb_state_new(xkb_keymap);
}
static void keyboard_enter(void *data,
                           struct wl_keyboard *kbd,
                           uint32_t serial,
                           struct wl_surface *surface,
                           struct wl_array *keys)
{
  (void)data;
  (void)kbd;
  (void)serial;
  (void)surface;
  (void)keys;
}
static void keyboard_leave(void *data,
                           struct wl_keyboard *kbd,
                           uint32_t serial,
                           struct wl_surface *surface)
{
  (void)data;
  (void)kbd;
  (void)serial;
  (void)surface;
}
static void keyboard_key(void *data,
                         struct wl_keyboard *kbd,
                         uint32_t serial,
                         uint32_t time,
                         uint32_t key,
                         uint32_t state)
{
  (void)data; (void)kbd; (void)serial; (void)time;

  uint32_t code = key + 8;

  xkb_keysym_t sym = xkb_state_key_get_one_sym(xkb_state, code);

  egl_key k = xkb_to_egl(sym);
  egl_mod m = xkb_mod_to_egl(xkb_state);
  char name[64];
  xkb_keysym_get_name(sym, name, sizeof(name));
  value vk = Val_int(k);
  value vm = Val_int(m);
  value vx = Val_int(mouse_x);
  value vy = Val_int(mouse_y);

  if (state == WL_KEYBOARD_KEY_STATE_PRESSED &&
      key_press_callback != default_callback)
    protect_callback4("key press",
      &key_press_callback, &vk, &vm, &vx, &vy);
  else if (key_release_callback != default_callback)
    protect_callback4("key release",
      &key_release_callback, &vk, &vm, &vx, &vy);
}
static void keyboard_modifiers(void *data,
                               struct wl_keyboard *kbd,
                               uint32_t serial,
                               uint32_t mods_depressed,
                               uint32_t mods_latched,
                               uint32_t mods_locked,
                               uint32_t group)
{
  if (!xkb_state)
    return;

  xkb_state_update_mask(xkb_state,
                        mods_depressed,
                        mods_latched,
                        mods_locked,
                        0,
                        0,
                        group);
}
static void keyboard_repeat_info(void *data,
                                 struct wl_keyboard *keyboard,
                                 int32_t rate,
                                 int32_t delay)
{
  (void)data;
  (void)keyboard;
  (void)rate;
  (void)delay;
}
static const struct wl_keyboard_listener keyboard_listener =
{
  keyboard_keymap,
  keyboard_enter,
  keyboard_leave,
  keyboard_key,
  keyboard_modifiers,
  keyboard_repeat_info
};

/* ========================= */
/* Pointer                  */
/* ========================= */
static void pointer_enter(void *data,
                          struct wl_pointer *pointer,
                          uint32_t serial,
                          struct wl_surface *surface,
                          wl_fixed_t sx,
                          wl_fixed_t sy)
{
    (void)data;
    (void)pointer;
    (void)serial;
    (void)surface;
    (void)sx;
    (void)sy;
}

static void pointer_leave(void *data,
                          struct wl_pointer *p,
                          uint32_t serial,
                          struct wl_surface *surface)
{
}

static void pointer_motion(void *data,
                           struct wl_pointer *p,
                           uint32_t time,
                           wl_fixed_t sx,
                           wl_fixed_t sy)
{
  mouse_x = wl_fixed_to_int(sx);
  mouse_y = wl_fixed_to_int(sy);

  if (motion_notify_callback != default_callback) {
    value vs = Val_int(xkb_mod_to_egl(xkb_state));
    value vx = Val_int(mouse_x);
    value vy = Val_int(mouse_y);
    protect_callback3("motion notify callback", &motion_notify_callback,
		      &vs, &vx, &vy);
  }
}
static void pointer_button(void *data,
                            struct wl_pointer *p,
                            uint32_t serial,
                            uint32_t time,
                            uint32_t button,
                            uint32_t state)
{
  egl_button b = EGL_BUTTON_Unknown;

  if (button == BTN_LEFT) b = EGL_BUTTON_Left;
  else if (button == BTN_RIGHT) b = EGL_BUTTON_Right;
  else if (button == BTN_MIDDLE) b = EGL_BUTTON_Middle;

  value vb = Val_int(b);
  value vs = Val_int(state);
  value vx = Val_int(mouse_x);
  value vy = Val_int(mouse_y);

  protect_callback4("mouse",
    &button_press_callback,
    &vb, &vs, &vx, &vy);
}

static void pointer_axis(void *data,
                         struct wl_pointer *p,
                         uint32_t time,
                         uint32_t axis,
                         wl_fixed_t value)
{
  (void)data; (void)p; (void)time; (void)axis; (void)value;
}
static void pointer_frame(void *data,
                          struct wl_pointer *p)
{
  (void)data; (void)p;
}
static const struct wl_pointer_listener pointer_listener =
{
    .enter  = pointer_enter,
    .leave  = pointer_leave,
    .motion = pointer_motion,
    .button = pointer_button,
    .axis   = pointer_axis,
    .frame       = pointer_frame,
    .axis_source = NULL,
    .axis_stop   = NULL,
    .axis_discrete = NULL
};



/* ========================= */
/* Seat                     */
/* ========================= */

static void seat_caps(void *data,
                      struct wl_seat *seat,
                      uint32_t caps)
{
  (void)data;

  if (caps & WL_SEAT_CAPABILITY_KEYBOARD)
  {
    wl_keyboard = wl_seat_get_keyboard(seat);
    wl_keyboard_add_listener(wl_keyboard,
                             &keyboard_listener, NULL);
  }

  if (caps & WL_SEAT_CAPABILITY_POINTER)
  {
    wl_pointer = wl_seat_get_pointer(seat);
    wl_pointer_add_listener(wl_pointer,
                            &pointer_listener, NULL);
  }
}
static void seat_name(void *data,
                      struct wl_seat *seat,
                      const char *name)
{
    (void)data;
    (void)seat;
    (void)name;
}
static const struct wl_seat_listener seat_listener =
{
  seat_caps,
  seat_name
};

static void wm_base_ping(void *data,
                         struct xdg_wm_base *wm_base,
                         uint32_t serial)
{
  xdg_wm_base_pong(wm_base, serial);
}

static const struct xdg_wm_base_listener wm_base_listener =
{
  .ping = wm_base_ping
};
static void xdg_surface_configure(void *data,
                                   struct xdg_surface *xdg_surface,
                                   uint32_t serial)
{
  xdg_surface_ack_configure(xdg_surface, serial);
}
static const struct xdg_surface_listener xdg_surface_listener =
{
  .configure = xdg_surface_configure
};

static void xdg_toplevel_configure(void *data,
                                   struct xdg_toplevel *toplevel,
                                   int32_t new_width,
                                   int32_t new_height,
                                   struct wl_array *states)
{
  /* width et height sont les nouvelles dimensions proposées
     par le compositeur.
     0 signifie "inchangé" dans certains cas. */
  uint32_t *s;
  int active = 0;
  printf("toplevel_configure");
  wl_array_for_each(s, states)
    {
      if (*s == XDG_TOPLEVEL_STATE_ACTIVATED)
	active = 1;
    }

  window_visible = active;
  if (new_width > 0 && new_height > 0) {

    wl_egl_window_resize(platform_window, width, height, 0, 0);

    if (width != new_width || height != new_height) {

      width  = new_width;
      height = new_height;
      wl_egl_window_resize(platform_window,
			   width,
			   height,
			   0, 0);

      value w = Val_int(width);
      value h = Val_int(height);

      protect_callback2("reshape callback",
			&reshape_callback,
			&w, &h);
    }
  }
}
static void xdg_toplevel_close(void *data,
                               struct xdg_toplevel *toplevel)
{
    (void)data;
    (void)toplevel;

    if (delete_callback == default_callback)
        main_loop_continue = 0;
    else {
        value u = Val_unit;
        protect_callback("delete callback",
                         &delete_callback,
                         &u);
    }
}
static const struct xdg_toplevel_listener xdg_toplevel_listener =
{
    xdg_toplevel_configure,
    xdg_toplevel_close
};

/* ========================= */
/* Init platform           */
/* ========================= */

void init_platform_ressources(int w, int h, const char* name)
{
  (void)name;

  platform_display = wl_display_connect(NULL);
  if (!platform_display)
    init_fail("wayland connect failed");

  wl_registry = wl_display_get_registry(platform_display);
  wl_registry_add_listener(wl_registry,
                           &registry_listener, NULL);

  wl_display_roundtrip(platform_display);

  if (!wl_compositor || !wl_seat)
    init_fail("missing compositor/seat");

  wl_surface = wl_compositor_create_surface(wl_compositor);

  platform_window = wl_egl_window_create(wl_surface, w, h);
  if (!platform_window)
    init_fail("egl window failed");

  xkb_ctx = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  if (!xkb_ctx)
    init_fail("xkb failed");

  wl_seat_add_listener(wl_seat, &seat_listener, NULL);

  xdg_wm_base_add_listener(xdg_wm_base, &wm_base_listener, NULL);
  xdg_surface = xdg_wm_base_get_xdg_surface(xdg_wm_base, wl_surface);
  xdg_surface_add_listener(xdg_surface,
			   &xdg_surface_listener,
			   NULL);

  xdg_toplevel = xdg_surface_get_toplevel(xdg_surface);

  xdg_toplevel_set_title(xdg_toplevel, name);
  xdg_toplevel_set_app_id(xdg_toplevel, name);
  xdg_toplevel_add_listener(xdg_toplevel,
			    &xdg_toplevel_listener,
			    NULL);
  wl_surface_commit(wl_surface);
  wl_display_flush(platform_display);
  wl_display_roundtrip(platform_display);

}

/* ========================= */
/* cleanup                 */
/* ========================= */

void free_platform_ressources()
{
  if (platform_window) wl_egl_window_destroy(platform_window);
  if (wl_surface) wl_surface_destroy(wl_surface);
  if (platform_display) wl_display_disconnect(platform_display);

  if (xkb_state) xkb_state_unref(xkb_state);
  if (xkb_keymap) xkb_keymap_unref(xkb_keymap);
  if (xkb_ctx) xkb_context_unref(xkb_ctx);
}

/* ========================= */
/* main loop               */
/* ========================= */
static const struct wl_callback_listener frame_listener;
static void frame_done(void *data,
                       struct wl_callback *frame_cb,
                       uint32_t time)
{
  wl_callback_destroy(frame_cb);
  frame_cb = wl_surface_frame(wl_surface);
  wl_callback_add_listener(frame_cb,
			   &frame_listener,
			   NULL);

  value u = Val_unit;
  protect_callback("idle",
		   &idle_callback,
		   &u);
}
static const struct wl_callback_listener frame_listener = {
	.done = frame_done,
};

void ml_egl_main_loop()
{
  CAMLparam0() ;
  if(!initialized)
    caml_failwith("Egl.main_loop: not initialized") ;

  if(main_loop_reentrant)
    caml_failwith("Egl.main_loop: forbidden reentrant call") ;

  caml_release_runtime_system();
  main_loop_continue = 1 ;
  main_loop_reentrant = 1 ;
  struct wl_callback *frame_cb = wl_surface_frame(wl_surface);
  wl_callback_add_listener(frame_cb,
			   &frame_listener,
			   NULL);
  value u = Val_unit;
  if (idle_callback != default_callback) {
    protect_callback("idle", &idle_callback, &u);
  }
  wl_surface_commit(wl_surface);
  wl_display_flush(platform_display);
  while (main_loop_continue) {
    wl_display_dispatch(platform_display);
  }
  main_loop_reentrant = 0 ;
  caml_acquire_runtime_system();
  CAMLreturn0 ;
}
