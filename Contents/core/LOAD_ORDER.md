# Load order

`PackageContents.xml` laduje aktualnie pliki w tej kolejnosci:

1. `Contents/gp_Config.lsp`
2. `Contents/gp_Core.lsp`
3. `Contents/core/gp_CoreRuntime.lsp`
4. `Contents/core/gp_ProjectMemory.lsp`
5. `Contents/core/gp_Numbering.lsp`
6. `Contents/core/gp_CadObjects.lsp`
7. `Contents/core/gp_Workgroups.lsp`
8. `Contents/core/gp_PikietaFactory.lsp`
9. `Contents/core/gp_PikietaStyle.lsp`
10. `Contents/ui/gp_SetupDialog.lsp`
11. `Contents/ui/gp_SetupAutosave.lsp`
12. `Contents/ui/gp_SetupPrefix.lsp`
13. `Contents/ui/gp_SetupGroup.lsp`
14. `Contents/ui/gp_SetupMain.lsp`
15. pozostale moduly funkcjonalne: import, export, wstawianie, niwelacje, siatka, usuwanie.

`Contents/core/gp_CoreLegacy.lsp` nie jest juz ladowany z manifestu.
Zostaje w repo jako kopia awaryjna do porownania albo szybkiego rollbacku.

Po tym etapie AutoCAD powinien korzystac z realnych modulow `core/` i `ui/`, a nie z monolitu legacy.
