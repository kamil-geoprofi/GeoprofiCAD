# GeoprofiCAD core

Ten folder zawiera logike ladowana przez `PackageContents.xml` po `Contents/gp_Core.lsp`.

Aktualny etap refaktoru jest zachowawczy:

- `Contents/gp_Core.lsp` jest stabilnym loaderem kompatybilnosci.
- `gp_CoreRuntime.lsp` jest technicznym punktem przejscia / orkiestratorem.
- `gp_CoreLegacy.lsp` zawiera jeszcze nierozdzielona implementacje starego core.
- Moduly `gp_ProjectMemory.lsp`, `gp_Numbering.lsp`, `gp_CadObjects.lsp`, `gp_PikietaFactory.lsp`, `gp_PikietaStyle.lsp` istnieja juz jako docelowe miejsca dla czystego podzialu.

Na razie moduly poza `gp_ProjectMemory.lsp` sa shellami typu shadow split. Legacy nadal daje pelna dzialajaca implementacje. Nastepny krok to przenoszenie definicji z `gp_CoreLegacy.lsp` do tych plikow bez zmian logiki, a dopiero potem cleanup legacy.

Docelowy podzial:

- `gp_ProjectMemory.lsp` — LDATA, ustawienia DWG, profile grup roboczych.
- `gp_Numbering.lsp` — prefixy i liczniki numeracji pikiet.
- `gp_CadObjects.lsp` — niskopoziomowe helpery AutoCAD/VLAX.
- `gp_PikietaFactory.lsp` — tworzenie i batchowe wstawianie pikiet.
- `gp_PikietaStyle.lsp` — konwersje Blok/Tekst i aktualizacja istniejacych pikiet.

Zasada refaktoru: najpierw przeniesienie bez zmian logiki, potem dopiero optymalizacje.
