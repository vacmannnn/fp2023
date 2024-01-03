package "Parser" (
  directory = "Parser"
  description = ""
  requires =
  "Lua.ast
   angstrom
   base
   ppx_expect.collector
   ppx_expect.config
   ppx_here.runtime-lib
   ppx_inline_test.config
   ppx_inline_test.runtime-lib"
  archive(byte) = "parser.cma"
  archive(native) = "parser.cmxa"
  plugin(byte) = "parser.cma"
  plugin(native) = "parser.cmxs"
)
package "ast" (
  directory = "ast"
  description = ""
  requires = "ppx_deriving.runtime"
  archive(byte) = "ast.cma"
  archive(native) = "ast.cmxa"
  plugin(byte) = "ast.cma"
  plugin(native) = "ast.cmxs"
)