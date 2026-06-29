# GeoprofiCAD core

Ten folder zawiera logike ladowana przez `Contents/gp_Core.lsp`.

Aktualny etap refaktoru jest zachowawczy: `gp_Core.lsp` zostal odchudzony do loadera, a dotychczasowa implementacja zostala zachowana w `gp_CoreRuntime.lsp`, zeby nie zmieniac publicznych nazw funkcji ani zachowania komend AutoCAD.

Docelowy podzial runtime:

- `gp_ProjectMemory.lsp` — LDATA, ustawienia DWG, profile grup roboczych.
- `gp_Numbering.lsp` — prefixy i liczniki numeracji pikiet.
- `gp_CadObjects.lsp` — niskopoziomowe helpery AutoCAD/VLAX.
- `gp_PikietaFactory.lsp` — tworzenie i batchowe wstawianie pikiet.
- `gp_PikietaStyle.lsp` — konwersje Blok/Tekst i aktualizacja istniejących pikiet.
