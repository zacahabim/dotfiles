return {
	"junegunn/seoul256.vim",
	name = "seoul256",
	config = function()
	    vim.cmd('let g:seoul256_background = 235')
	    vim.cmd('silent! colorscheme seoul256')
  	end,
}
