# GeoprofiCAD UI

Ten folder zawiera kod UI/DCL, przede wszystkim obsluge `GEO_SETUP`.

UI zostalo wydzielone z dawnego monolitu `gp_Core.lsp`. `gp_CoreLegacy.lsp` nie jest juz ladowany z `PackageContents.xml`, wiec `GEO_SETUP` dziala z modulow w tym folderze.

## Moduly UI

- `gp_SetupDialog.lsp` — podstawowe helpery UI oraz komenda `c:GEO_SETUP`.
- `gp_SetupAutosave.lsp` — autosave i walidacja pol `txt_h`, `z_prec`, stylu, widocznosci i koloru.
- `gp_SetupPrefix.lsp` — lista i aktywacja prefixow numeracji w `GEO_SETUP`.
- `gp_SetupGroup.lsp` — ladowanie grupy, tworzenie nowej grupy i dialog nowego prefixu numeracji.
- `gp_SetupMain.lsp` — glowne okno `GEO_SETUP`, w tym `geocad-setup-show-main-dialog`.

## Zasady

- Kod UI zostaje w `Contents/ui/`.
- Logika wspolna i operacje na obiektach CAD zostaja w `Contents/core/`.
- `GEO_SETUP` powinien wywolywac funkcje core, ale nie powinien zawierac niskopoziomowej logiki CAD, jezeli da sie ja wydzielic.
