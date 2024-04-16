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

-- reset syntax highlighting
map("n", "<leader>hh", ":syntax sync fromstart<CR>", "Refresh syntax highlight")

-- neotree
map("n", "-", ":Neotree<CR>", "Open Neo-tree")

-- fast moving
map("n", "H", "10h", "Quick h")
map("n", "J", "10j", "Quick j")
map("n", "K", "10k", "Quick k")
map("n", "L", "10l", "Quick l")

map("v", "H", "10h", "Quick h")
map("v", "J", "10j", "Quick j")
map("v", "K", "10k", "Quick k")
map("v", "L", "10l", "Quick l")
