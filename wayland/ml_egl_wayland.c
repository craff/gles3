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

typedef struct platform_context_struct {
  struct wl_registry   *wl_registry;
  struct wl_compositor *wl_compositor;
  struct wl_surface    *wl_surface;

  struct wl_seat      *wl_seat;
  struct wl_keyboard  *wl_keyboard;
  struct wl_pointer   *wl_pointer;

  struct xdg_wm_base *xdg_wm_base;
  struct xdg_surface *xdg_surface;
  struct xdg_toplevel *xdg_toplevel;

  struct xkb_context *xkb_ctx;
  struct xkb_keymap   *xkb_keymap;
  struct xkb_state    *xkb_state;
  int   mouse_x;
  int   mouse_y;
  int   window_visible;
} *platform_context;

platform_context malloc_platform_context(egl_context ctxt) {
  platform_context pctxt = (platform_context) malloc(sizeof(struct platform_context_struct));
  if (!pctxt) init_fail(ctxt, "can not allocate platform_context");
  bzero(pctxt, sizeof(struct platform_context_struct));
  return pctxt;
}
/* ========================= */
/* XKB                      */
/* ========================= */


/* ========================= */
/* Registry                 */
/* ========================= */

static void registry_global(void *data,
                            struct wl_registry *registry,
                            uint32_t name,
                            const char *interface,
                            uint32_t version)
{
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;

  if (strcmp(interface, "wl_compositor") == 0)
    pctxt->wl_compositor = wl_registry_bind(registry, name,
                                     &wl_compositor_interface, 4);

  if (strcmp(interface, "wl_seat") == 0)
    pctxt->wl_seat = wl_registry_bind(registry, name,
                               &wl_seat_interface, 7);

  if (strcmp(interface, "xdg_wm_base") == 0)
    pctxt->xdg_wm_base = wl_registry_bind(registry, name,
				     &xdg_wm_base_interface, 1);
}
static void registry_remove(void *, struct wl_registry *, uint32_t) {}
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
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;

  char *map = mmap(NULL, size, PROT_READ, MAP_SHARED, fd, 0);

  pctxt->xkb_keymap =
    xkb_keymap_new_from_string(pctxt->xkb_ctx,
			       map,
			       XKB_KEYMAP_FORMAT_TEXT_V1,
			       XKB_KEYMAP_COMPILE_NO_FLAGS);

  munmap(map, size);
  close(fd);

  pctxt->xkb_state = xkb_state_new(pctxt->xkb_keymap);
}
static void keyboard_enter(void *, struct wl_keyboard *, uint32_t,  struct wl_surface *, struct wl_array *) {}
static void keyboard_leave(void *, struct wl_keyboard *, uint32_t,  struct wl_surface *) {}
static void keyboard_key(void *data,
                         struct wl_keyboard *kbd,
                         uint32_t serial,
                         uint32_t time,
                         uint32_t key,
                         uint32_t state)
{
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;

  uint32_t code = key + 8;

  xkb_keysym_t sym = xkb_state_key_get_one_sym(pctxt->xkb_state, code);

  egl_key k = xkb_to_egl(sym);
  egl_mod m = xkb_mod_to_egl(pctxt->xkb_state);
  char name[64];
  xkb_keysym_get_name(sym, name, sizeof(name));
  value vk = Val_int(k);
  value vm = Val_int(m);
  value vx = Val_int(pctxt->mouse_x);
  value vy = Val_int(pctxt->mouse_y);

  if (state == WL_KEYBOARD_KEY_STATE_PRESSED &&
      ctxt->key_press_callback != Val_unit)
    protect_callback4("key press",
		      &(ctxt->key_press_callback), &vk, &vm, &vx, &vy);
  else if (ctxt->key_release_callback != Val_unit)
    protect_callback4("key release",
		      &(ctxt->key_release_callback), &vk, &vm, &vx, &vy);
}
static void keyboard_modifiers(void *data,
                               struct wl_keyboard *kbd,
                               uint32_t serial,
                               uint32_t mods_depressed,
                               uint32_t mods_latched,
                               uint32_t mods_locked,
                               uint32_t group)
{
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;

  if (!pctxt->xkb_state)
    return;

  xkb_state_update_mask(pctxt->xkb_state,
                        mods_depressed,
                        mods_latched,
                        mods_locked,
                        0,
                        0,
                        group);
}
static void keyboard_repeat_info(void *, struct wl_keyboard *, int32_t,  int32_t)
{
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

static void pointer_motion(void *data,
                           struct wl_pointer *p,
                           uint32_t time,
                           wl_fixed_t sx,
                           wl_fixed_t sy)
{
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;
  pctxt->mouse_x = wl_fixed_to_int(sx);
  pctxt->mouse_y = wl_fixed_to_int(sy);

  if (ctxt->motion_notify_callback != Val_unit) {
    value vs = Val_int(xkb_mod_to_egl(pctxt->xkb_state));
    value vx = Val_int(pctxt->mouse_x);
    value vy = Val_int(pctxt->mouse_y);
    protect_callback3("motion notify callback",
		      &(ctxt->motion_notify_callback),
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
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;
  egl_button b = EGL_BUTTON_Unknown;

  if (button == BTN_LEFT) b = EGL_BUTTON_Left;
  else if (button == BTN_RIGHT) b = EGL_BUTTON_Right;
  else if (button == BTN_MIDDLE) b = EGL_BUTTON_Middle;

  value vb = Val_int(b);
  value vs = Val_int(state);
  value vx = Val_int(pctxt->mouse_x);
  value vy = Val_int(pctxt->mouse_y);

  if (state == WL_POINTER_BUTTON_STATE_PRESSED
      && ctxt->button_press_callback != Val_unit)
    protect_callback4("mouse",
		      &(ctxt->button_press_callback),
		      &vb, &vs, &vx, &vy);
  else if (ctxt->button_release_callback != Val_unit)
    protect_callback4("mouse",
		      &(ctxt->button_release_callback),
		      &vb, &vs, &vx, &vy);
}
static void pointer_enter(void *, struct wl_pointer *, uint32_t,  struct wl_surface *, wl_fixed_t,  wl_fixed_t) {}
static void pointer_leave(void *, struct wl_pointer *, uint32_t,  struct wl_surface *) {}
static void pointer_axis(void *, struct wl_pointer *, uint32_t,  uint32_t,  wl_fixed_t) {}
static void pointer_frame(void *, struct wl_pointer *) {}
static void pointer_axis_source(void *, struct wl_pointer *, uint32_t) {}
static void pointer_axis_stop(void *, struct wl_pointer *, uint32_t,  uint32_t) {}
static void pointer_axis_discrete(void *, struct wl_pointer *, uint32_t,  int32_t) {}
static const struct wl_pointer_listener pointer_listener =
{
    .enter  = pointer_enter,
    .leave  = pointer_leave,
    .motion = pointer_motion,
    .button = pointer_button,
    .axis   = pointer_axis,
    .frame       = pointer_frame,
    .axis_source = pointer_axis_source,
    .axis_stop   = pointer_axis_stop,
    .axis_discrete = pointer_axis_discrete
};



/* ========================= */
/* Seat                     */
/* ========================= */

static void seat_caps(void *data,
                      struct wl_seat *seat,
                      uint32_t caps)
{
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;

  if (caps & WL_SEAT_CAPABILITY_KEYBOARD)
  {
    pctxt->wl_keyboard = wl_seat_get_keyboard(seat);
    wl_keyboard_add_listener(pctxt->wl_keyboard,
                             &keyboard_listener, data);
  }

  if (caps & WL_SEAT_CAPABILITY_POINTER)
  {
    pctxt->wl_pointer = wl_seat_get_pointer(seat);
    wl_pointer_add_listener(pctxt->wl_pointer,
                            &pointer_listener, data);
  }
}
static void seat_name(void *, struct wl_seat *, const char *) {}
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
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;
  /* width et height sont les nouvelles dimensions proposées
     par le compositeur.
     0 signifie "inchangé" dans certains cas. */
  uint32_t *s;
  int active = 0;
  wl_array_for_each(s, states)
    {
      if (*s == XDG_TOPLEVEL_STATE_ACTIVATED)
	active = 1;
    }

  pctxt->window_visible = active;
  if (new_width > 0 && new_height > 0) {

    if (ctxt->width != new_width || ctxt->height != new_height) {

      ctxt->width  = new_width;
      ctxt->height = new_height;
      wl_egl_window_resize(ctxt->platform_window,
			   ctxt->width,
			   ctxt->height,
			   0, 0);

      if (ctxt->reshape_callback != Val_unit) {
	value w = Val_int(ctxt->width);
	value h = Val_int(ctxt->height);

	protect_callback2("reshape callback",
			  &(ctxt->reshape_callback),
			  &w, &h);
      }
    }
  }
}
static void xdg_toplevel_close(void *data,
                               struct xdg_toplevel *toplevel)
{
  egl_context ctxt = (egl_context) data;
  if (ctxt->delete_callback == Val_unit)
    ctxt->main_loop_continue = 0;
  else {
    value u = Val_unit;
    protect_callback("delete callback",
		     &(ctxt->delete_callback),
		     &u);
  }
}
void xdg_toplevel_configure_bounds(void *, struct xdg_toplevel *, int32_t,  int32_t) {}
void xdg_toplevel_wm_capabilities(void *, struct xdg_toplevel *, struct wl_array *) {}
//static void xdg_toplevel_configure_maximized(...) {}
static const struct xdg_toplevel_listener xdg_toplevel_listener =
{
    xdg_toplevel_configure,
    xdg_toplevel_close,
    xdg_toplevel_configure_bounds,
    xdg_toplevel_wm_capabilities
};

/* ========================= */
/* Init platform           */
/* ========================= */

void init_platform_ressources(egl_context ctxt, const char* name)
{
  ctxt->platform_display = wl_display_connect(NULL);
  if (!ctxt->platform_display || !ctxt->platform)
    init_fail(ctxt, "wayland connect failed");

  platform_context pctxt = ctxt->platform;

  pctxt->wl_registry = wl_display_get_registry(ctxt->platform_display);
  wl_registry_add_listener(pctxt->wl_registry,
                           &registry_listener, (void *) ctxt);

  wl_display_roundtrip(ctxt->platform_display);

  if (!pctxt->wl_compositor || !pctxt->wl_seat || !pctxt->xdg_wm_base)
    init_fail(ctxt, "missing compositor/seat/xdg_wm_base");

  pctxt->wl_surface = wl_compositor_create_surface(pctxt->wl_compositor);

  ctxt->platform_window = wl_egl_window_create(pctxt->wl_surface,
					       ctxt->width, ctxt->height);
  if (!ctxt->platform_window)
    init_fail(ctxt,"egl window failed");

  pctxt->xkb_ctx = xkb_context_new(XKB_CONTEXT_NO_FLAGS);
  if (!pctxt->xkb_ctx)
    init_fail(ctxt, "xkb failed");

  wl_seat_add_listener(pctxt->wl_seat, &seat_listener, ctxt);

  xdg_wm_base_add_listener(pctxt->xdg_wm_base, &wm_base_listener, ctxt);
  pctxt->xdg_surface = xdg_wm_base_get_xdg_surface(pctxt->xdg_wm_base,
						   pctxt->wl_surface);
  xdg_surface_add_listener(pctxt->xdg_surface,
			   &xdg_surface_listener,
			   ctxt);

  pctxt->xdg_toplevel = xdg_surface_get_toplevel(pctxt->xdg_surface);

  xdg_toplevel_set_title(pctxt->xdg_toplevel, name);
  xdg_toplevel_set_app_id(pctxt->xdg_toplevel, name);
  xdg_toplevel_add_listener(pctxt->xdg_toplevel,
			    &xdg_toplevel_listener,
			    ctxt);
  wl_surface_commit(pctxt->wl_surface);
  wl_display_flush(ctxt->platform_display);
  wl_display_roundtrip(ctxt->platform_display);

}

/* ========================= */
/* cleanup                 */
/* ========================= */

void free_platform_ressources(egl_context ctxt)
{
    platform_context pctxt = ctxt->platform;

    if (!pctxt) return;
    if (pctxt->xdg_toplevel) xdg_toplevel_destroy(pctxt->xdg_toplevel);
    if (pctxt->xdg_surface) xdg_surface_destroy(pctxt->xdg_surface);
    if (pctxt->xkb_ctx) xkb_context_unref(pctxt->xkb_ctx);
    if (ctxt->platform_window) wl_egl_window_destroy(ctxt->platform_window);
    if (pctxt->wl_surface) wl_surface_destroy(pctxt->wl_surface);
    if (pctxt->wl_pointer) wl_pointer_destroy(pctxt->wl_pointer);
    if (pctxt->xkb_state) xkb_state_unref(pctxt->xkb_state);
    if (pctxt->xkb_keymap) xkb_keymap_unref(pctxt->xkb_keymap);
    if (pctxt->wl_keyboard) wl_keyboard_destroy(pctxt->wl_keyboard);
    if (pctxt->xdg_wm_base) xdg_wm_base_destroy(pctxt->xdg_wm_base);
    if (pctxt->wl_compositor) wl_compositor_destroy(pctxt->wl_compositor);
    if (pctxt->wl_seat) wl_seat_destroy(pctxt->wl_seat);
    if (pctxt->wl_registry) wl_registry_destroy(pctxt->wl_registry);
    if (ctxt->platform_display) wl_display_disconnect(ctxt->platform_display);

    free(pctxt);
    ctxt->platform = NULL;
}

/* ========================= */
/* main loop               */
/* ========================= */
static const struct wl_callback_listener frame_listener;
static void frame_done(void *data,
                       struct wl_callback *frame_cb,
                       uint32_t time)
{
  egl_context ctxt = (egl_context) data;
  platform_context pctxt = ctxt->platform;
  wl_callback_destroy(frame_cb);
  frame_cb = wl_surface_frame(pctxt->wl_surface);
  wl_callback_add_listener(frame_cb,
			   &frame_listener,
			   data);

  value u = Val_unit;
  if (ctxt->idle_callback != Val_unit) {
    protect_callback("idle",
		     &(ctxt->idle_callback),
		     &u);
  }
}
static const struct wl_callback_listener frame_listener = {
	.done = frame_done,
};

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
  ctxt->main_loop_continue = 1 ;
  platform_context pctxt = ctxt->platform;
  struct wl_callback *frame_cb = wl_surface_frame(pctxt->wl_surface);
  wl_callback_add_listener(frame_cb,
			   &frame_listener,
			   ctxt);
  wl_surface_commit(pctxt->wl_surface);
  wl_display_flush(ctxt->platform_display);
  while (ctxt->main_loop_continue) {
    wl_display_dispatch(ctxt->platform_display);
  }
  caml_acquire_runtime_system();
  atomic_store(&ctxt->main_loop_reentrant, 0) ;
  CAMLreturn(Val_unit) ;
}
