local opts = {
	listchars = "nbsp:¬,tab:>-,extends:»,precedes:«,trail:•",
	list = true,
	encoding = "utf-8",
	fileencoding = "utf-8",
	fileformat= "unix",
	hidden = true,
	autoread = true,
	backspace = "indent,eol,start",
	incsearch = true,
	hlsearch = true,
	ignorecase = true,
	smartcase = true,
	modifiable = true,
	backup = false,
	writebackup = true,
	swapfile = false,
	expandtab = true,
	autoindent = true,
	smartindent = true,
	shiftwidth = 4,
	tabstop = 4,
	softtabstop = 4,
	scrolloff = 5,
	wrap = true,
	termguicolors = true,
	number = true,
	relativenumber = true,
	wildmenu = true,
	wildmode = "longest:full,full",
	colorcolumn = "80",
	laststatus = 2,
	splitbelow = true,
	splitright = true,
	background = 'dark',
}

-- Set options from table
for opt, val in pairs(opts) do
	vim.api.nvim_set_option_value(opt, val, {})
end

-- Force to use OSC 52 provider for clipboard
vim.g.clipboard = {
  name = 'OSC 52',
  copy = {
    ['+'] = require('vim.ui.clipboard.osc52').copy('+'),
    ['*'] = require('vim.ui.clipboard.osc52').copy('*'),
  },
  paste = {
    ['+'] = require('vim.ui.clipboard.osc52').paste('+'),
    ['*'] = require('vim.ui.clipboard.osc52').paste('*'),
  },
}
