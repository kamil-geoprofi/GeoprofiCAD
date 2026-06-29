# GeoprofiCAD core

Ten folder zawiera wspolna logike GeoprofiCAD ladowana z `PackageContents.xml` po `Contents/gp_Core.lsp`.

`Contents/gp_Core.lsp` zostaje stabilnym punktem kompatybilnosci dla starszych plikow, ktore nadal moga robic `(load "gp_Core.lsp")`. Realna implementacja jest rozdzielona na moduly w `Contents/core/` i `Contents/ui/`.

`gp_CoreLegacy.lsp` nadal istnieje w repo jako nieaktywny backup po refaktorze, ale nie jest juz ladowany przez `PackageContents.xml`.

## Moduly core

- `gp_CoreRuntime.lsp` — lekki runtime/orchestrator marker.
- `gp_ProjectMemory.lsp` — LDATA, ustawienia DWG, `geocad-get-cfg`, `geocad-set-cfg`.
- `gp_Numbering.lsp` — prefixy numeracji, liczniki i `GP:PobierzNastepnyNumer`.
- `gp_CadObjects.lsp` — niskopoziomowe helpery AutoCAD/VLAX, obiekty CAD, punkty i teksty.
- `gp_Workgroups.lsp` — grupy robocze, skaner warstw, pamiec grup i inicjalizacja ustawien DWG.
- `gp_PikietaSchema.lsp` — centralny schemat pikiety: nazwa bloku, tag numeru, tagi rzednej i wartosci domyslne.
- `gp_TextRadar.lsp` — wspolny radar tekstow: bounding box, klasyfikacja ID/Z i parowanie tekstow z punktami bez `ssget` w petli.
- `gp_PikietaFactory.lsp` — bazowe tworzenie bloku, kontekst wstawiania i batchowe wstawianie pikiet.
- `gp_PikietaStyle.lsp` — bazowe funkcje stylu pikiet.
- `gp_PikietaData.lsp` — model posredni konwersji: DWG -> `PikietaData[]` -> zapis wariantu docelowego.
- `gp_PikietaStyleOptimized.lsp` — runtime schema + publiczne nadpisania konwersji i auto-apply, przekierowane na `gp_PikietaData.lsp`.
- `gp_PikietaTextFast.lsp` — szybkie tworzenie tekstowych pikiet zgodne z importem: `entmakex`, bez COM Visible dla nowych tekstow.
- `gp_ExportRadarCompat.lsp` — kompatybilne wrappery dla starego radaru w `gp_Export.lsp`, przekierowane do `gp_TextRadar.lsp`.

## Zasady

- Logika wspolna dla importu, eksportu i wstawiania powinna trafiac do `core/`.
- Logika okien i `GEO_SETUP` powinna trafiac do `Contents/ui/`.
- `PackageContents.xml` jest zrodlem prawdy dla kolejnosci ladowania.
- Schemat pikiety powinien byc czytany z `gp_PikietaSchema.lsp`; nie wpisujemy recznie `Pikieta_Geo`, `NR`, `H`, `Z`, `RZEDNA` w nowych miejscach.
- Konwersje stylu pikiet powinny isc przez model posredni `PikietaData`, a nie mieszac odczytu, zapisu i modyfikacji obiektow w jednej petli.
