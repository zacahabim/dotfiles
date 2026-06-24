-- Highlight, edit, and navigate code
return {
	{
		"nvim-treesitter/nvim-treesitter",
		build = ":TSUpdate",
		config = function()
			require("nvim-treesitter").setup({})
			-- Install parsers
			local installed = require("nvim-treesitter.config").get_installed()
			local ensure = { "c", "cpp", "go", "lua", "python", "rust", "vimdoc", "vim" }
			local to_install = vim.tbl_filter(function(lang)
				return not vim.list_contains(installed, lang)
			end, ensure)
			if #to_install > 0 then
				require("nvim-treesitter.install").install(to_install)
			end

			-- Enable treesitter highlight and indent (built-in)
			vim.api.nvim_create_autocmd("FileType", {
				callback = function(args)
					pcall(vim.treesitter.start, args.buf)
				end,
			})
		end,
	},
	{
		"nvim-treesitter/nvim-treesitter-textobjects",
		branch = "main",
		dependencies = { "nvim-treesitter/nvim-treesitter" },
		config = function()
			require("nvim-treesitter-textobjects").setup({
				select = { lookahead = true },
				move = { set_jumps = true },
			})

			-- Select keymaps
			local select_fn = function(query)
				return function()
					require("nvim-treesitter-textobjects.select").select_textobject(query, "textobjects")
				end
			end
			vim.keymap.set({ "x", "o" }, "aa", select_fn("@parameter.outer"))
			vim.keymap.set({ "x", "o" }, "ia", select_fn("@parameter.inner"))
			vim.keymap.set({ "x", "o" }, "af", select_fn("@function.outer"))
			vim.keymap.set({ "x", "o" }, "if", select_fn("@function.inner"))
			vim.keymap.set({ "x", "o" }, "ac", select_fn("@class.outer"))
			vim.keymap.set({ "x", "o" }, "ic", select_fn("@class.inner"))

			-- Move keymaps
			local move = require("nvim-treesitter-textobjects.move")
			vim.keymap.set({ "n", "x", "o" }, "]m", function()
				move.goto_next_start("@function.outer", "textobjects")
			end)
			vim.keymap.set({ "n", "x", "o" }, "]]", function()
				move.goto_next_start("@class.outer", "textobjects")
			end)
			vim.keymap.set({ "n", "x", "o" }, "]M", function()
				move.goto_next_end("@function.outer", "textobjects")
			end)
			vim.keymap.set({ "n", "x", "o" }, "][", function()
				move.goto_next_end("@class.outer", "textobjects")
			end)
			vim.keymap.set({ "n", "x", "o" }, "[m", function()
				move.goto_previous_start("@function.outer", "textobjects")
			end)
			vim.keymap.set({ "n", "x", "o" }, "[[", function()
				move.goto_previous_start("@class.outer", "textobjects")
			end)
			vim.keymap.set({ "n", "x", "o" }, "[M", function()
				move.goto_previous_end("@function.outer", "textobjects")
			end)
			vim.keymap.set({ "n", "x", "o" }, "[]", function()
				move.goto_previous_end("@class.outer", "textobjects")
			end)
		end,
	},
}
