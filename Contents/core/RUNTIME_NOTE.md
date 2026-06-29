# Runtime scaffold

`gp_CoreRuntime.lsp` is intentionally small in this stage.

The bundle manifest loads runtime scaffold, full legacy core, and then split modules. This keeps existing AutoLISP commands stable while the code is moved into modules.
