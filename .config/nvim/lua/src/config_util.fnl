(when (not (. table :unpack)) (set table.unpack (. _G :unpack)))

(fn setn [tbl & args]
  (each [k v (pairs args)]
    (tset tbl k v)))

(fn call [tbl item & args]
  (local fun (. tbl item))
  (fun (table.unpack args)))

(local config_name (require "current_config"))
(fn req [name]
  (require (.. config_name "." name)))

{:call call :setn setn :req req}
