# GeoprofiCAD core

Ten folder zawiera logike ladowana przez `Contents/gp_Core.lsp` oraz `PackageContents.xml`.

Aktualny etap refaktoru jest zachowawczy:

- `Contents/gp_Core.lsp` jest stabilnym loaderem kompatybilności.
- `gp_CoreRuntime.lsp` jest technicznym punktem przejścia / orkiestratorem.
- `gp_CoreLegacy.lsp` zawiera jeszcze nierozdzieloną implementację starego core.

Docelowo zawartość `gp_CoreLegacy.lsp` będzie przenoszona bez zmian zachowania do mniejszych plików:

- `gp_ProjectMemory.lsp` — LDATA, ustawienia DWG, profile grup roboczych.
- `gp_Numbering.lsp` — prefixy i liczniki numeracji pikiet.
- `gp_CadObjects.lsp` — niskopoziomowe helpery AutoCAD/VLAX.
- `gp_PikietaFactory.lsp` — tworzenie i batchowe wstawianie pikiet.
- `gp_PikietaStyle.lsp` — konwersje Blok/Tekst i aktualizacja istniejących pikiet.

Zasada refaktoru: najpierw przeniesienie bez zmian logiki, potem dopiero optymalizacje.
