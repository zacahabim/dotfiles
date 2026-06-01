-- LSP Configuration & Plugins
return {
	{
		"neovim/nvim-lspconfig",
		dependencies = {
			"williamboman/mason.nvim",
			"williamboman/mason-lspconfig.nvim",
			{
				"j-hui/fidget.nvim",
				tag = "legacy",
				event = "LspAttach",
			},
			"folke/neodev.nvim",
			"RRethy/vim-illuminate",
			"hrsh7th/cmp-nvim-lsp",
			"stevearc/dressing.nvim",
		},
		config = function()
			require("mason").setup()
			require("mason-lspconfig").setup({
				ensure_installed = {
					"ansiblels",
					"bashls",
					"gopls",
					"helm_ls",
					"lua_ls",
					"pyright",
					"yamlls",
				},
				automatic_installation = true,
			})

			require("helpers.keys").map("n", "<leader>M", "<cmd>Mason<cr>", "Show Mason")
			require("neodev").setup()
			require("fidget").setup()

			-- Diagnostic signs
			local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
			for type, icon in pairs(signs) do
				local hl = "DiagnosticSign" .. type
				vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
			end

			vim.diagnostic.config({
				virtual_text = false,
				signs = { active = signs },
				update_in_insert = true,
				underline = true,
				severity_sort = true,
				float = {
					focusable = true,
					style = "minimal",
					border = "rounded",
					source = "always",
					header = "",
					prefix = "",
				},
			})

			-- Keybindings on LSP attach
			vim.api.nvim_create_autocmd("LspAttach", {
				callback = function(ev)
					local bufnr = ev.buf
					local client = vim.lsp.get_client_by_id(ev.data.client_id)
					local lsp_map = require("helpers.keys").lsp_map

					lsp_map("<leader>le", vim.diagnostic.open_float, bufnr, "Open diagnostics")
					lsp_map("[d", vim.diagnostic.goto_prev, bufnr, "Goto Prev diagnostics")
					lsp_map("]d", vim.diagnostic.goto_next, bufnr, "Goto Next diagnostics")
					lsp_map("<leader>lq", vim.diagnostic.setloclist, bufnr, "Set location list")
					lsp_map("<leader>lr", vim.lsp.buf.rename, bufnr, "Rename symbol")
					lsp_map("<leader>la", vim.lsp.buf.code_action, bufnr, "Code action")
					lsp_map("<leader>ld", vim.lsp.buf.type_definition, bufnr, "Type definition")
					lsp_map("<leader>ls", vim.lsp.buf.document_symbol, bufnr, "Document symbols")
					lsp_map("gr", vim.lsp.buf.references, bufnr, "Goto References")
					lsp_map("gd", vim.lsp.buf.definition, bufnr, "Goto Definition")
					lsp_map("gD", vim.lsp.buf.declaration, bufnr, "Goto Declaration")
					lsp_map("gi", vim.lsp.buf.implementation, bufnr, "Goto Implementation")
					lsp_map("gk", vim.lsp.buf.hover, bufnr, "Hover Documentation")

					vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
						vim.lsp.buf.format()
					end, { desc = "Format current buffer with LSP" })
					lsp_map("<leader>cf", "<cmd>Format<cr>", bufnr, "Format")

					if client then
						require("illuminate").on_attach(client)
					end
				end,
			})

			-- Capabilities for nvim-cmp
			local capabilities = require("cmp_nvim_lsp").default_capabilities()

			-- Configure servers with vim.lsp.config (Neovim 0.11+ API)
			vim.lsp.config("*", {
				capabilities = capabilities,
			})

			vim.lsp.config("lua_ls", {
				settings = {
					Lua = {
						completion = { callSnippet = "Replace" },
						diagnostics = { globals = { "vim" } },
						workspace = {
							library = {
								[vim.fn.expand("$VIMRUNTIME/lua")] = true,
								[vim.fn.stdpath("config") .. "/lua"] = true,
							},
						},
					},
				},
			})

			vim.lsp.enable({ "ansiblels", "bashls", "gopls", "helm_ls", "lua_ls", "pyright", "yamlls" })
		end,
	},
}
