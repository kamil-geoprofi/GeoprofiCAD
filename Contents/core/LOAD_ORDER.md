# Load order

`PackageContents.xml` laduje aktualnie pliki w tej kolejnosci:

1. `Contents/gp_Config.lsp`
2. `Contents/gp_Core.lsp`
3. `Contents/core/gp_CoreRuntime.lsp`
4. `Contents/core/gp_ProjectMemory.lsp`
5. `Contents/core/gp_Numbering.lsp`
6. `Contents/core/gp_CadObjects.lsp`
7. `Contents/core/gp_Workgroups.lsp`
8. `Contents/core/gp_TextRadar.lsp`
9. `Contents/core/gp_PikietaFactory.lsp`
10. `Contents/core/gp_PikietaStyle.lsp`
11. `Contents/core/gp_PikietaStyleOptimized.lsp`
12. `Contents/ui/gp_SetupDialog.lsp`
13. `Contents/ui/gp_SetupAutosave.lsp`
14. `Contents/ui/gp_SetupPrefix.lsp`
15. `Contents/ui/gp_SetupGroup.lsp`
16. `Contents/ui/gp_SetupMain.lsp`
17. `Contents/gp_Import.lsp`
18. `Contents/gp_Export.lsp`
19. `Contents/core/gp_ExportRadarCompat.lsp`
20. pozostale moduly funkcjonalne: wstawianie, niwelacje, siatka, usuwanie.

`Contents/core/gp_CoreLegacy.lsp` nie jest juz ladowany z manifestu.
Zostaje w repo jako kopia awaryjna do porownania albo szybkiego rollbacku.

`gp_PikietaStyleOptimized.lsp` musi byc ladowany po `gp_PikietaStyle.lsp`, bo nadpisuje wybrane funkcje konwersji.
`gp_ExportRadarCompat.lsp` musi byc ladowany po `gp_Export.lsp`, bo przekierowuje stare helpery eksportu na wspolny `gp_TextRadar.lsp`.
