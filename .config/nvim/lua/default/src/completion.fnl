(local lspconfig (require :lspconfig))
(local cmp (require :cmp))

(local homedir vim.env.HOME)

;; this gets the lsp to shut up
(when (not (. vim.uv :fs_stat))
  (set vim.uv.fs_stat (lambda [] nil)))

(vim.diagnostic.config {:virtual_text false}) ;; no messages inline

(vim.api.nvim_set_keymap "n" "E"
  ":lua vim.diagnostic.open_float(nil, {focus = false})<CR>"
  {:noremap true})
(vim.api.nvim_set_keymap "n" "<C-e>"
  ":lua vim.diagnostic.goto_next()<CR>"
  {:noremap true})
(vim.api.nvim_set_keymap "n" "<M-e>"
  ":lua vim.diagnostic.goto_prev()<CR>"
  {:noremap true})

(fn on_attach_fn [client bufnr]
    (local _ client)
    (fn buf_set_keymap [...] (vim.api.nvim_buf_set_keymap bufnr ...))
    (local opts {:noremap true :silent true})
    (buf_set_keymap "n" "gd"
      "<cmd>lua vim.lsp.buf.definition()<CR>"
      opts)
    (buf_set_keymap "n" "K"
      "<cmd>lua vim.lsp.buf.hover()<CR>"
      opts)
    (buf_set_keymap "n" "gr"
      "<cmd>lua vim.lsp.buf.references()<CR>"
      opts))

(lspconfig.teal_ls.setup
  {:on_attach on_attach_fn
   :cmd ["teal-language-server"]
   :filetypes [:teal]})
;;(vim.lsp.enable "teal_ls")

(lspconfig.nelua_lsp.setup
  {:on_attach on_attach_fn
   :cmd ["nelua" "-L" (.. homedir "/OPT/src/nelua-lsp/") "--script" (.. homedir "/OPT/src/nelua-lsp/nelua-lsp.lua")]})

(lspconfig.hls.setup
  {:on_attach on_attach_fn
   :settings
   {:haskell
    {:plugin
     {:hlint
      {;; disables annoying refactor suggestions
       :diagnosticsOn false
       :codeActionsOn false}}}}})

(lspconfig.gopls.setup {:on_attach on_attach_fn})

(lspconfig.zls.setup {:on_attach on_attach_fn})
(set vim.g.zig_fmt_autosave 0) ;; disable auto formatting on save

(lspconfig.serve_d.setup
  {:cmd ["serve-d"]
   :filetypes [:d]
   :on_attach on_attach_fn})

(lspconfig.erlangls.setup
  {:cmd ["erlang_ls"]
   :filetypes [:erlang]
   :on_attach on_attach_fn})

(lspconfig.elixirls.setup
  {:cmd ["elixir-ls"]
   :filetypes [:elixir]
   :on_attach on_attach_fn})

(lspconfig.nim_langserver.setup
  {:cmd [(.. homedir "/BIN/nimlangserver")]
   :filetypes [:nim]
   :settings
   {:nim
    {:timeout 6000000
     :nimsuggestTimeout 6000000
     :nimsuggestIdleTimeout 6000000
     :notificationVerbosity :error
     :logNimsuggest false}}
    :on_attach on_attach_fn})

(lspconfig.nixd.setup {})

(lspconfig.ols.setup
  {:cmd [(.. homedir "/OPT/bin/ols")]
   :on_attach on_attach_fn})

(lspconfig.ocamllsp.setup
  {:cmd ["ocamllsp"]
   :filetypes [:ocaml :menhir :ocamlinterface :ocamllex :reason :dune]
   :on_attach on_attach_fn})

(lspconfig.v_analyzer.setup
  {:cmd [(.. homedir "/.config/v-analyzer/bin/v-analyzer")]
   :filetypes [:v :vv :vsh :vlang]
   :on_attach on_attach_fn})

(lspconfig.fennel_ls.setup
  { ;; fennel-ls requires flsproject.fnl
   :cmd ["fennel-ls"]
   :filetypes [:fennel]
   :root_dir (lspconfig.util.root_pattern :fnl :src :fennel)
   :on_attach on_attach_fn})

;; this doesn't require a project file, but
;; doesn't allow for arbitrary globals and docs like fennel_ls does
;; (which makes it less convenient for editing neovim or love2d projects):
;;
;;(tset (require "lspconfig.configs") :fennel_language_server
;;  {:default_config
;;   {:cmd ["/PATH/TO/BINFILE"]
;;    :filetypes [:fennel]
;;    :single_file_support true
;;    :root_dir (lspconfig.util.root_pattern :fnl :src :fennel)
;;    :settings
;;    {:fennel
;;     {:workspace {:library (vim.api.nvim_list_runtime_paths)}
;;      :diagnostics {:globals [:vim]}}}}})
;;
;;(lspconfig.fennel_language_server.setup
;;  {:cmd [:fennel-language-server]
;;   :filetypes [:fennel]
;;   :root_dir (lspconfig.util.root_pattern :fnl :src :fennel)
;;   :on_attach on_attach_fn})

(lspconfig.gleam.setup
  {:cmd ["gleam" "lsp"]
   :filetypes [:gleam]
   :on_attach on_attach_fn})

(lspconfig.ada_ls.setup {:on_attach on_attach_fn})

(lspconfig.clojure_lsp.setup
  {:cmd ["clojure-lsp"]
   :filetypes [:clojure :edn]
   :on_attach on_attach_fn})

(lspconfig.clangd.setup
  {:cmd ["clangd"]
   :filetypes [:c :cpp]
   :on_attach on_attach_fn})

(lspconfig.rust_analyzer.setup
  {:cmd ["rust-analyzer"]
   :filetypes [:rust]
   :root_dir (lspconfig.util.root_pattern ".git" "Cargo.toml")
   :on_attach  on_attach_fn})

(lspconfig.omnisharp.setup
  {:cmd ["omnisharp"]
   :filetypes [:cs]
   :on_attach on_attach_fn})

(lspconfig.fsharp_language_server.setup
  {:cmd ["fsautocomplete" "--adaptive-lsp-server-enabled"]
   :filetypes [:fsharp]
   :on_attach on_attach_fn})

;; NOTE:
;; kotlin-language-server from nix tends to work more consistently than either
;; compiling from source or from AUR.
;; Remember to delete your kls_database.db every time you need new dependencies
;; in gradle :(
(lspconfig.kotlin_language_server.setup
  {:cmd ["kotlin-language-server"]
   :root_dir (lspconfig.util.root_pattern
               ".git" "build.gradle.kts"
               "build.gradle" "pom.xml"
               "settings.gradle" "settings.gradle.kts")
   :on_attach on_attach_fn})

(lspconfig.jdtls.setup
  {:cmd ["jdtls"]
   :filetypes [:java]
   :on_attach on_attach_fn})

(lspconfig.sourcekit.setup
  {:cmd ["sourcekit-lsp"]
   :filetypes [:swift :objc :objcpp] ;; can also be used for c/c++
   :root_dir (lspconfig.util.root_pattern "Package.swift" ".git") ;; Package.swift is generated by `swift package init`
   :capabilities
   {:workspace
    {:didChangeWatchedFiles
     {:dynamicRegistration true}}}
   :on_attach on_attach_fn})

(lspconfig.solargraph.setup
  {:cmd ["solargraph" "stdio"]
   :filetypes [:ruby]
   :root_dir (lspconfig.util.root_pattern "Gemfile" "Gemfile.lock" ".git" ".ruby-lsp")
   :on_attach on_attach_fn})

(lspconfig.racket_langserver.setup
  {:cmd ["racket" "--lib" "racket-langserver"]
   :filetypes [:racket :scheme]
   :on_attach on_attach_fn})

(lspconfig.haxe_language_server.setup
  {:cmd ["node" (.. homedir "/BIN/haxe-language-server.js")] ;; vshaxe's haxe-language-server (build it from github)
   :filetypes [:haxe :hxml]
   :on_attach on_attach_fn})

(lspconfig.pylsp.setup
  {:cmd ["pylsp"]
   :filetypes [:python]
   :on_attach on_attach_fn})

(lspconfig.ocamlls.setup
  {:cmd ["ocaml-language-server" "--stdio"]
   :filetypes [:ocaml :reason]
   :on_attach on_attach_fn})

(lspconfig.janet_lsp.setup
  {:cmd ["janet-lsp" "--stdio"]
   :filetypes [:janet]
   :on_attach on_attach_fn})

(lspconfig.lua_ls.setup
  {:cmd ["lua-language-server"]
   :filetypes [:lua]
   :on_attach on_attach_fn
   :on_init (lambda [client]
     (when client.workspace_folders
       (local path (. (. client.workspace_folders 1) :name))
       (when (or (vim.uv.fs_stat (.. path "/.luarc.json"))
                  (vim.uv.fs_stat (.. path "/.luarc.jsonc")))
         (lua "return")))
     (set client.config.settings.Lua
       (vim.tbl_deep_extend "force" client.config.settings.Lua
         {:runtime {:version :LuaJIT}
          :workspace
          {:checkThirdParty false
           :library
           [vim.env.VIMRUNTIME
            "${3rd}/love2d/library"]}})))
    :settings {:Lua {}}})

(tset (require "lspconfig.configs") :hy
  {:default_config
   {:cmd ["hyuga"]
   :filetypes [:hy]
   :root_dir (lambda [fname]
     (lspconfig.util.path.dirname fname))}
   :docs
   {:description
    "Hyuga language server for the Hy programming language"}})

(lspconfig.hy.setup
  {:cmd ["hyuga"]
   :filetypes [:hy]
   :on_attach on_attach_fn})

(tset (require "lspconfig.configs") :moonscript_language_server
    {:default_config
     {:cmd ["/PATH/TO/BINFILE"]
      :filetypes [:moon :moonscript]
      :single_file_support true
      :root_dir (lspconfig.util.root_pattern :moon ".git")}})

(lspconfig.moonscript_language_server.setup
  {:cmd ["moonscript-language-server"]
   :filetypes [:moon :moonscript]
   :on_attach on_attach_fn})

(cmp.setup
  {:snippet
   {:expand (lambda [args]
      ((. (require "luasnip") :lsp_expand) args.body))} ;; For LuaSnip users.
   :mapping
   {"<C-n>" (cmp.mapping.select_next_item)
    "<C-p>" (cmp.mapping.select_prev_item)
    "<C-y>" (cmp.mapping.confirm {:select true})}
   :sources
   [{:name :nvim_lsp}
    {:name :buffer}
    {:name :path}]})

(cmp.setup.cmdline ":" {:sources [{:name :cmdline}]})
