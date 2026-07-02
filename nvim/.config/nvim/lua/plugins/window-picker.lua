return {
  "s1n7ax/nvim-window-picker",
  name = "window-picker",
  event = "VeryLazy",
  version = "2.*",
  opts = {
    hint = "floating-big-letter",
    filter_rules = {
      include_current_win = false,
      autoselect_one = true,
      -- Filter rules for window picker
      bo = {
        filetype = { "neo-tree", "neo-tree-popup", "notify", "oil" },
        buftype = { "terminal", "quickfix" },
      },
      filetype = { "neo-tree", "neo-tree-popup", "notify", "oil" },
      buftype = { "terminal", "quickfix" },
    },
  },
}
