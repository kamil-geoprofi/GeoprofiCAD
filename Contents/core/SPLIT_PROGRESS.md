# Split progress

## Zrobione w tym etapie

- Utworzono stabilny loader `Contents/gp_Core.lsp`.
- Utworzono runtime/orchestrator `Contents/core/gp_CoreRuntime.lsp`.
- Przeniesiono pelny dotychczasowy core do `Contents/core/gp_CoreLegacy.lsp`.
- Utworzono docelowe moduly:
  - `gp_ProjectMemory.lsp`
  - `gp_Numbering.lsp`
  - `gp_CadObjects.lsp`
  - `gp_PikietaFactory.lsp`
  - `gp_PikietaStyle.lsp`
  - `ui/gp_SetupDialog.lsp`
- Dodano je do `PackageContents.xml` po legacy.
- `gp_ProjectMemory.lsp` zawiera juz rzeczywiste definicje konfiguracji i LDATA.
- Pozostale moduly maja mapy API i markery ladowania, ale definicje funkcji nadal sa w legacy.

## Dlaczego tak

To zabezpiecza ladowanie bundle i pozwala testowac strukture przed wycinaniem definicji z legacy. Po potwierdzeniu, ze AutoCAD laduje wszystkie nowe pliki, nastepny etap to przenoszenie funkcji modul po module.

## Nastepny krok

Przeniesc definicje funkcji do:

1. `gp_Numbering.lsp`
2. `gp_CadObjects.lsp`
3. `gp_PikietaFactory.lsp`
4. `gp_PikietaStyle.lsp`
5. `ui/gp_SetupDialog.lsp`

Dopiero po testach odchudzac albo usuwac `gp_CoreLegacy.lsp`.
