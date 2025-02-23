-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- This is where you actually apply your config choices

-- For example, changing the color scheme:
config.color_scheme = 'AdventureTime'
config.font = wezterm.font 'JetBrains Mono'
config.font_size = 14
config.window_background_opacity = 1.0
config.text_background_opacity = 1.0
config.warn_about_missing_glyphs = false
config.enable_scroll_bar = true
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = true
config.enable_kitty_keyboard = true
config.keys = {
  {
    key = '_',
    mods = 'CTRL|SHIFT',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'ALT',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'OPT',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'META',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'ALT | SHIFT',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'OPT | SHIFT',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Enter',
    mods = 'META | SHIFT',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = '@',
    mods = 'CTRL',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Tab',
    mods = 'CTRL',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'Tab',
    mods = 'CTRL | SHIFT',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'w',
    mods = 'CMD',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'n',
    mods = 'CMD',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'p',
    mods = 'CMD',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'f',
    mods = 'CMD',
    action = wezterm.action.DisableDefaultAssignment,
  },
  {
    key = 'b',
    mods = 'CMD',
    action = wezterm.action.DisableDefaultAssignment,
  },
}

-- and finally, return the configuration to wezterm
return config
