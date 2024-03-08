local map = require("helpers.keys").map

-- Diagnostic keymaps
map('n', 'gx', vim.diagnostic.open_float, "Show diagnostics under cursor")

-- Switch between light and dark modes
map("n", "<leader>ut", function()
	if vim.o.background == "dark" then
		vim.o.background = "light"
	else
		vim.o.background = "dark"
	end
end, "Toggle between light and dark themes")

-- quickfix
map("n", "]q", ":cnext<CR>", "Next global quickfix")
map("n", "[q", ":cprev<CR>", "Previous global quickfix")
map("n", "]l", ":lnext<CR>", "Next local quickfix")
map("n", "[l", ":lprev<CR>", "Previous local quickfix")
map("n", "]b", ":bnext<CR>", "Next buffer")
map("n", "[b", ":bprev<CR>", "Previous buffer")

map("n", "<leader>hh", "<silent> :syntax sync fromstart<CR>", "Refresh syntax highlight")

-- neo-tree
map("n", "-", ":Neotree<CR>", "Open Neo-tree")
