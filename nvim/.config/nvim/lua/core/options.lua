local opts = {
	listchars = "nbsp:¬,tab:>-,extends:»,precedes:«,trail:•",
	list = true,
	-- timeoutlen = 8000,
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
	nobackup = true,
	writebackup = true,
	noswapfile = true,
	nobackup = true,
	writebackup = true,
	noswapfile = true,
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
	colorcolumn = 80,
	laststatus = 2,
	splitbelow = true,
	splitright = true,
}

-- Set options from table
for opt, val in pairs(opts) do
	vim.o[opt] = val
end

-- Set other options
local colorscheme = require("helpers.colorscheme")
vim.cmd.colorscheme(colorscheme)
