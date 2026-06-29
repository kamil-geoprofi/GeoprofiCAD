# Runtime scaffold

`gp_CoreRuntime.lsp` is intentionally small in this stage.

The bundle manifest loads:

1. runtime scaffold,
2. full legacy core,
3. split modules.

This keeps existing AutoLISP commands stable while the code is moved into modules.
