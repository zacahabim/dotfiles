-- Pull in the wezterm API
local wezterm = require 'wezterm'

-- This will hold the configuration.
local config = wezterm.config_builder()

-- local variable act for wezterm.action

local act = wezterm.action

-- session manager

local session_manager = require("wezterm-session-manager/session-manager")
wezterm.on("save_session", function(window) session_manager.save_state(window) end)
wezterm.on("load_session", function(window) session_manager.load_state(window) end)
wezterm.on("restore_session", function(window) session_manager.restore_state(window) end)

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

config.leader = { key = 'ö', mods = 'CTRL', timeout_milliseconds = 5000 }
config.unix_domains = {
  {
    name = 'unix',
  },
}

config.keys = {
  -- mux
  --session manager
  {key = "S", mods = "LEADER", action = wezterm.action{EmitEvent = "save_session"}},
  {key = "L", mods = "LEADER", action = wezterm.action{EmitEvent = "load_session"}},
  {key = "R", mods = "LEADER", action = wezterm.action{EmitEvent = "restore_session"}},
  -- Attach to muxer
  {
    key = 'a',
    mods = 'LEADER',
    action = act.AttachDomain 'unix',
  },
  -- Detach from muxer
  {
    key = 'd',
    mods = 'LEADER',
    action = act.DetachDomain { DomainName = 'unix' },
  },
  {
    key = '$',
    mods = 'LEADER',
    action = act.PromptInputLine {
      description = 'Enter new name for session',
      action = wezterm.action_callback(
        function(window, pane, line)
          if line then
            mux.rename_workspace(
              window:mux_window():get_workspace(),
              line
            )
          end
        end
      ),
    },
  },
  -- Show list of workspaces
  {
    key = 's',
    mods = 'LEADER',
    action = act.ShowLauncherArgs { flags = 'WORKSPACES' },
  },
  -- splitting
  {
    mods   = "LEADER",
    key    = "\"",
    action = act.SplitVertical { domain = 'CurrentPaneDomain' }
  },
  {
    mods   = "LEADER",
    key    = "%",
    action = act.SplitHorizontal { domain = 'CurrentPaneDomain' }
  },
  {
    mods   = "LEADER",
    key    = "|",
    action = act.SplitPane {
      direction = 'Right',
      top_level = true,
      size = { Percent = 50 },
    }
  },
  {
    mods   = "LEADER",
    key    = "_",
    action = act.SplitPane {
      direction = 'Down',
      top_level = true,
      size = { Percent = 50 },
    }
  },
  -- reorder
  {
    mods = "LEADER",
    key = "Space",
    action = act.RotatePanes "Clockwise"
  },
  -- show the pane selection mode, but have it swap the active and selected panes
  {
    mods = 'LEADER',
    key = '0',
    action = act.PaneSelect {
      mode = 'SwapWithActive',
    },
  },
  -- rename tab
  {
    key = ',',
    mods = "LEADER",
    action = act.PromptInputLine {
      description = 'Enter new name for tab',
      action = wezterm.action_callback(function(window, pane, line)
        -- line will be `nil` if they hit escape without entering anything
        -- An empty string if they just hit enter
        -- Or the actual line of text they wrote
        if line then
          window:active_tab():set_title(line)
        end
      end),
    },
  },
  -- move pane to new tab
  {
    key = '!',
    mods = 'LEADER',
    action = wezterm.action_callback(function(win, pane)
      local tab, window = pane:move_to_new_tab()
    end),
  },
  -- tab navigation
  {key="t", mods="LEADER", action=act{ActivateTabRelative=1}},
  {key="T", mods="LEADER", action=act{ActivateTabRelative=-1}},
  {
    key = 'z',
    mods = 'LEADER',
    action = act.TogglePaneZoomState,
  },
  {
    key = '&',
    mods = 'LEADER',
    action = act.CloseCurrentTab{ confirm = true },
  },
  {
    key = 'c',
    mods = 'LEADER',
    action = act.SpawnTab('DefaultDomain')
  },
  -- pane navigation
  {
      key = 'LeftArrow',
      mods = 'LEADER',
      action = act.ActivatePaneDirection 'Left',
  },
  {
      key = 'DownArrow',
      mods = 'LEADER',
      action = act.ActivatePaneDirection 'Down',
  },
  {
      key = 'UpArrow',
      mods = 'LEADER',
      action = act.ActivatePaneDirection 'Up',
  },
  {
      key = 'RightArrow',
      mods = 'LEADER',
      action = act.ActivatePaneDirection 'Right',
  },
  {
    key = 'Space',
    mods = 'LEADER',
    action = act.RotatePanes 'CounterClockwise',
  },
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

-- tab navigation
for i = 1, 9 do
    table.insert(
        config.keys,
        {
        key = tostring(i),
        mods = "LEADER",
        action = act.ActivateTab(i - 1),
        }
    )
end

-- and finally, return the configuration to wezterm
return config
