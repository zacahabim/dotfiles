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
			-- Set up Mason before anything else
			require("mason").setup()
			require("mason-lspconfig").setup({
				ensure_installed = {
					"ansiblels",
					"bashls",
					"gopls",
					"helm_ls",
					"pyright",
					"yamlls",
				},
				automatic_installation = true,
			})

			-- Quick access via keymap
			require("helpers.keys").map("n", "<leader>M", "<cmd>Mason<cr>", "Show Mason")

			-- Neodev setup before LSP config
			require("neodev").setup()

			-- Turn on LSP status information
			require("fidget").setup()

			-- Set up cool signs for diagnostics
			local signs = { Error = " ", Warn = " ", Hint = " ", Info = " " }
			for type, icon in pairs(signs) do
				local hl = "DiagnosticSign" .. type
				vim.fn.sign_define(hl, { text = icon, texthl = hl, numhl = "" })
			end

			-- Diagnostic config
			local config = {
				virtual_text = false,
				signs = {
					active = signs,
				},
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
			}
			vim.diagnostic.config(config)

			-- This function gets run when an LSP connects to a particular buffer.
			local on_attach = function(client, bufnr)
				local lsp_map = require("helpers.keys").lsp_map

				lsp_map("<leader>le", vim.diagnostic.open_float, bufnr, "Open diagnostics")
				lsp_map("[d", vim.diagnostic.goto_prev, bufnr, "Goto Next diagnostics")
				lsp_map("]d", vim.diagnostic.goto_next, bufnr, "Goto Prev diagnostics")
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

				-- Create a command `:Format` local to the LSP buffer
				vim.api.nvim_buf_create_user_command(bufnr, "Format", function(_)
					vim.lsp.buf.format()
				end, { desc = "Format current buffer with LSP" })

				lsp_map("<leader>cf", "<cmd>Format<cr>", bufnr, "Format")

				-- Attach and configure vim-illuminate
				require("illuminate").on_attach(client)
			end

			-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
			local capabilities = vim.lsp.protocol.make_client_capabilities()
			capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

			-- Lua
			require("lspconfig")["lua_ls"].setup({
				on_attach = on_attach,
				capabilities = capabilities,
				settings = {
					Lua = {
						completion = {
							callSnippet = "Replace",
						},
						diagnostics = {
							globals = { "vim" },
						},
						workspace = {
							library = {
								[vim.fn.expand("$VIMRUNTIME/lua")] = true,
								[vim.fn.stdpath("config") .. "/lua"] = true,
							},
						},
					},
				},
			})
			require("lspconfig")["pyright"].setup({
				on_attach = on_attach,
				capabilities = capabilities,
			})
			require("lspconfig")["yamlls"].setup({
				on_attach = on_attach,
				capabilities = capabilities,
			})
		end
	},
}
