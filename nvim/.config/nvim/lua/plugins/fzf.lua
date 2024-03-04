-- Git related plugins
return {
	{
		"junegunn/fzf",
	},
	{
		"junegunn/fzf.vim",
		keys = {
			{ "<leader>ff", "<cmd>Files<cr>", desc = "Search all files"},
			{ "<leader>gf", "<cmd>GFiles<cr>", desc = "Search Git files"},
			{ "<leader>bb", "<cmd>Buffers<cr>", desc = "Search all buffers"},
		},
	},
}
