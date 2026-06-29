# GeoprofiCAD core load order

`PackageContents.xml` jest zrodlem prawdy dla kolejnosci ladowania.

1. `Contents/core/gp_CoreRuntime.lsp`
2. `Contents/core/gp_ProjectMemory.lsp`
3. `Contents/core/gp_Numbering.lsp`
4. `Contents/core/gp_CadObjects.lsp`
5. `Contents/core/gp_Workgroups.lsp`
6. `Contents/core/gp_PikietaSchema.lsp`
7. `Contents/core/gp_TextRadar.lsp`
8. `Contents/core/gp_PikietaWriters.lsp`
9. `Contents/core/gp_PikietaData.lsp`
10. `Contents/core/gp_PikietaFactory.lsp`
11. `Contents/core/gp_PikietaConversion.lsp`
12. `Contents/ui/gp_SetupDialog.lsp`
13. `Contents/ui/gp_SetupAutosave.lsp`
14. `Contents/ui/gp_SetupPrefix.lsp`
15. `Contents/ui/gp_SetupGroup.lsp`
16. `Contents/ui/gp_SetupMain.lsp`
17. `Contents/gp_Import.lsp`
18. `Contents/gp_Export.lsp`
19. `Contents/core/gp_ExportRadarCompat.lsp`
20. pozostale moduly funkcjonalne: wstawianie, niwelacje, siatka, usuwanie.

## Zaleznosci pikiet

`gp_PikietaSchema.lsp` musi byc ladowany przed modulami pikiet, bo definiuje nazwe bloku, tag numeru, tagi rzednej i fallbacki.

`gp_TextRadar.lsp` musi byc ladowany przed `gp_PikietaData.lsp`, bo odczyt wariantu tekstowego paruje punkty z tekstami przez wspolny radar.

`gp_PikietaWriters.lsp` musi byc ladowany przed `gp_PikietaFactory.lsp` i `gp_PikietaConversion.lsp`, bo jest jedynym miejscem tworzenia pikiety jako tekst albo blok.

`gp_PikietaData.lsp` musi byc ladowany przed `gp_PikietaConversion.lsp`, bo konwersje pracuja na pipeline `DWG -> PikietaData[] -> writer`.

`gp_PikietaFactory.lsp` odpowiada za kontekst, import/wstawianie i batch. Nie zawiera wlasnej logiki `entmakex`/`vla-InsertBlock` dla pikiet, tylko deleguje do writerow.

`gp_PikietaConversion.lsp` udostepnia publiczne operacje zmiany stylu i auto-apply dla UI.

`gp_PikietaStyle.lsp` i `gp_PikietaStyleOptimized.lsp` zostaly usuniete z aktywnego kodu. Ich odpowiedzialnosci przejely czytelne moduly: `gp_PikietaWriters.lsp`, `gp_PikietaData.lsp`, `gp_PikietaFactory.lsp` i `gp_PikietaConversion.lsp`.
