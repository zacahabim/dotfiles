return {
	{
		"folke/which-key.nvim",
		config = function()
			local wk = require("which-key")
			wk.setup()
			wk.add(
				{
					{"<leader>f" , group = "File" },
					{"<leader>d" , group = "Delete/Close" },
					{"<leader>q" , group = "Quit" },
					{"<leader>s" , group = "Search" },
					{"<leader>l" , group = "LSP" },
					{"<leader>u" , group = "UI" },
					{"<leader>b" , group = "Debugging" },
					{"<leader>g" , group = "Git" },
				}
			)
		end
	}
}
