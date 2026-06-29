# Split progress

## Aktualny stan

Refaktor przeszedl z trybu `shadow split` do pracy bez ladowania legacy:

- `Contents/gp_Core.lsp` jest stabilnym loaderem kompatybilnosci.
- `Contents/core/gp_CoreRuntime.lsp` jest malym runtime/orchestrator markerem.
- `Contents/core/gp_CoreLegacy.lsp` nadal istnieje w repo, ale nie jest juz ladowany przez `PackageContents.xml`.
- AutoCAD powinien teraz korzystac z realnych modulow `core/` i `ui/`.
- Pelne `GEO_SETUP` po przeniesieniu do modulow UI zostalo uruchomione i potwierdzone w AutoCAD przed wylaczeniem legacy.

## Realnie wydzielone moduly core

- `Contents/core/gp_ProjectMemory.lsp` — LDATA, konfiguracja DWG, `geocad-get-cfg`, `geocad-set-cfg`.
- `Contents/core/gp_Numbering.lsp` — prefixy numeracji, liczniki, `GP:PobierzNastepnyNumer`.
- `Contents/core/gp_CadObjects.lsp` — helpery AutoCAD/VLAX, punkty, teksty, nearest text.
- `Contents/core/gp_Workgroups.lsp` — pamiec grup roboczych, skaner warstw, inicjalizacja ustawien DWG.
- `Contents/core/gp_PikietaFactory.lsp` — tworzenie bloku, kontekst, batch insert pikiet.
- `Contents/core/gp_PikietaStyle.lsp` — konwersje Blok/Tekst i update istniejacych pikiet.

## Realnie wydzielone moduly UI

- `Contents/ui/gp_SetupDialog.lsp` — podstawowe helpery UI oraz `c:GEO_SETUP`.
- `Contents/ui/gp_SetupAutosave.lsp` — autosave/walidacja `txt_h`, `z_prec`, stylu, widocznosci i koloru.
- `Contents/ui/gp_SetupPrefix.lsp` — prefixy numeracji w `GEO_SETUP`.
- `Contents/ui/gp_SetupGroup.lsp` — ladowanie grupy, tworzenie grupy, dialog nowego prefixu.
- `Contents/ui/gp_SetupMain.lsp` — helpery glownego okna oraz pelne `geocad-setup-show-main-dialog`.

## Celowo jeszcze nie robione

- Fizyczne skasowanie albo odchudzenie `gp_CoreLegacy.lsp` — narzedzie zablokowalo masowa podmiane pliku; legacy zostaje jako nieaktywny backup w repo.
- Optymalizacja konwersji Blok/Tekst — jeszcze nie robiona.
- Radar tekstow z eksportu — jeszcze nie przeniesiony.

## Zasada dalszych zmian

1. Najpierw test AutoCAD bez ladowania legacy.
2. Jezeli wszystko dziala, mozna traktowac split jako stabilny.
3. Dopiero potem optymalizacje `ssget`, konwersji i przeniesienie radaru tekstow z eksportu.
