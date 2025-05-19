;;(tset (require "nvim-treesitter.install") :compilers ["gcc" "clang"])
((. (require "nvim-treesitter.configs") :setup)
 {:ensure_installed
  ["racket"
	 "c"
	 "c_sharp"
	 "commonlisp"
	 "cpp"
	 "css"
	 "bash"
	 "fennel"
	 "haskell"
	 "janet_simple"
	 "java"
	 "javascript"
	 "json"
	 "lua"
	 "ocaml"
	 "perl"
	 "php"
	 "python"
	 "ruby"
	 "scheme"
	 "rust"
	 "typescript"
	 "v"
	 "vim"
	 "zig"
	 "teal"]
  :highlight
  {:enable true
   :additional_vim_regex_highlighting false}
  :rainbow
  {:enable true
   :extended_mode true}})
