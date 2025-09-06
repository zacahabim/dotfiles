-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- local variable act for wezterm.action

local act = wezterm.action

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'Atom'
config.font = wezterm.font 'JetBrains Mono'
config.font_size = 14
config.window_background_opacity = 1.0
config.text_background_opacity = 1.0
config.warn_about_missing_glyphs = false
config.enable_scroll_bar = true
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = true

config.enable_kitty_keyboard = false
config.term = "xterm-256color"
config.hide_tab_bar_if_only_one_tab = true

config.window_padding = {
  bottom = 0,
}
config.scrollback_lines = 100000

config.tab_bar_at_bottom = true
config.use_fancy_tab_bar = true
config.pane_focus_follows_mouse = true

config.unix_domains = {
  {
    name = 'unix',
  },
}

config.keys = {
  -- disable default keybindings
  {
    key = '_',
    mods = 'CTRL|SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'ALT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'OPT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'META',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'ALT | SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'OPT | SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'META | SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = '@',
    mods = 'CTRL',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'Tab',
    mods = 'CTRL',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'Tab',
    mods = 'CTRL | SHIFT',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'w',
    mods = 'CMD',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'n',
    mods = 'CMD',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'p',
    mods = 'CMD',
    action = act.DisableDefaultAssignment,
  },
  {
    key = 'b',
    mods = 'CMD',
    action = act.DisableDefaultAssignment,
  },
}
-- and finally, return the configuration to wezterm
return config
