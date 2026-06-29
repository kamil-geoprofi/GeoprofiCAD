# Load order

`PackageContents.xml` laduje aktualnie pliki w tej kolejnosci:

1. `Contents/gp_Config.lsp`
2. `Contents/gp_Core.lsp`
3. `Contents/core/gp_CoreRuntime.lsp`
4. `Contents/core/gp_ProjectMemory.lsp`
5. `Contents/core/gp_Numbering.lsp`
6. `Contents/core/gp_CadObjects.lsp`
7. `Contents/core/gp_Workgroups.lsp`
8. `Contents/core/gp_PikietaSchema.lsp`
9. `Contents/core/gp_TextRadar.lsp`
10. `Contents/core/gp_PikietaFactory.lsp`
11. `Contents/core/gp_PikietaStyle.lsp`
12. `Contents/core/gp_PikietaData.lsp`
13. `Contents/core/gp_PikietaStyleOptimized.lsp`
14. `Contents/ui/gp_SetupDialog.lsp`
15. `Contents/ui/gp_SetupAutosave.lsp`
16. `Contents/ui/gp_SetupPrefix.lsp`
17. `Contents/ui/gp_SetupGroup.lsp`
18. `Contents/ui/gp_SetupMain.lsp`
19. `Contents/gp_Import.lsp`
20. `Contents/gp_Export.lsp`
21. `Contents/core/gp_ExportRadarCompat.lsp`
22. pozostale moduly funkcjonalne: wstawianie, niwelacje, siatka, usuwanie.

`Contents/core/gp_CoreLegacy.lsp` nie jest juz ladowany z manifestu.
Zostaje w repo jako kopia awaryjna do porownania albo szybkiego rollbacku.

`gp_PikietaSchema.lsp` musi byc ladowany przed factory/style/data, bo definiuje nazwe bloku i tagi atrybutow.
`gp_PikietaData.lsp` musi byc ladowany po `gp_PikietaStyle.lsp`, bo korzysta z istniejacych funkcji tworzenia wariantow pikiet.
`gp_PikietaStyleOptimized.lsp` musi byc ladowany po `gp_PikietaData.lsp`, bo publiczne operacje stylu kieruje na model posredni `PikietaData` i runtime schematu.
`gp_ExportRadarCompat.lsp` musi byc ladowany po `gp_Export.lsp`, bo przekierowuje stare helpery eksportu na wspolny `gp_TextRadar.lsp`.
