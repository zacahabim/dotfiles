return {
  "stevearc/oil.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    "s1n7ax/nvim-window-picker",
  },
  opts = {
    -- Oil will take over directory buffers (e.g. foo/bar/)
    default_file_explorer = true,
    -- Buffer-local options
    buf_options = {
      buflisted = false,
      bufhidden = "hide",
    },
    -- Window-local options
    win_options = {
      wrap = false,
      signcolumn = "no",
      cursorcolumn = false,
      foldcolumn = "0",
      spell = false,
      list = false,
      conceallevel = 3,
      concealcursor = "nvic",
    },
    -- Keymaps within oil buffer
    keymaps = {
      ["g?"] = "actions.show_help",
      ["<CR>"] = "actions.select",
      ["<C-j>"] = {
        callback = function()
          local oil = require("oil")
          local entry = oil.get_cursor_entry()
          if entry and entry.type ~= "directory" then
            local picker = require("window-picker")
            local picked_window_id = picker.pick_window()
            if picked_window_id then
              oil.select({
                handle_buffer_callback = function(buf_id)
                  vim.api.nvim_win_set_buf(picked_window_id, buf_id)
                  vim.api.nvim_set_current_win(picked_window_id)
                end,
              })
            end
          elseif entry and entry.type == "directory" then
            oil.select()
          end
        end,
        desc = "Open file in a picked window using window-picker",
      },
      ["<C-s>"] = { "actions.select", opts = { vertical = true }, desc = "Open the entry in a vertical split" },
      ["<C-h>"] = { "actions.select", opts = { horizontal = true }, desc = "Open the entry in a horizontal split" },
      ["<C-t>"] = { "actions.select", opts = { tab = true }, desc = "Open the entry in a new tab" },
      ["<C-p>"] = "actions.preview",
      ["<C-c>"] = "actions.close",
      ["<C-l>"] = "actions.refresh",
      ["-"] = "actions.parent",
      ["_"] = "actions.open_cwd",
      ["`"] = "actions.cd",
      ["~"] = { "actions.cd", opts = { scope = "tab" }, desc = "Change directory to the directory of the current tab" },
      ["gs"] = "actions.change_sort",
      ["gx"] = "actions.open_external",
      ["g."] = "actions.toggle_hidden",
      ["g\\"] = "actions.toggle_trash",
    },
    -- Set to false to disable all of the above keymaps
    use_default_keymaps = true,
    view_options = {
      -- Show files and directories that start with "."
      show_hidden = true,
      -- This function defines what is considered a "hidden" file
      is_hidden_file = function(name, bufnr)
        return vim.startswith(name, ".")
      end,
      -- This function defines what is considered a "ignored" file
      is_always_hidden = function(name, bufnr)
        return false
      end,
    },
  },
}
