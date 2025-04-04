return {
    'nvim-telescope/telescope.nvim', tag = '0.1.8',
    dependencies = { 'nvim-lua/plenary.nvim' },
	config = function()
	local builtin = require('telescope.builtin')
	vim.keymap.set('n', '<leader>ff', builtin.find_files, {})
	vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
	vim.keymap.set('n', '<leader>bb', builtin.buffers, {})
	vim.keymap.set('n', '<leader>gf', builtin.git_files, {})
	local actions = require('telescope.actions')require('telescope').setup{
	    pickers = {
		buffers = {
			sort_lastused = true
		}
	    }
	}
	end,
}
