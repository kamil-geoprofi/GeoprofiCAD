# Split progress

## Aktualny stan

Refaktor jest nadal w trybie `shadow split`:

- `Contents/gp_Core.lsp` jest stabilnym loaderem kompatybilnosci.
- `Contents/core/gp_CoreRuntime.lsp` jest malym runtime/orchestrator markerem.
- `Contents/core/gp_CoreLegacy.lsp` nadal zawiera pelna stara implementacje i jest ladowany jako bufor bezpieczenstwa.
- Nowe moduly sa ladowane po legacy i nadpisuja wybrane definicje bez zmiany publicznego API.
- Pelne `GEO_SETUP` po przeniesieniu do modulow UI zostalo uruchomione i potwierdzone w AutoCAD.

## Realnie wydzielone moduly core

- `Contents/core/gp_ProjectMemory.lsp` — LDATA, konfiguracja DWG, `geocad-get-cfg`, `geocad-set-cfg`.
- `Contents/core/gp_Numbering.lsp` — prefixy numeracji, liczniki, `GP:PobierzNastepnyNumer`.
- `Contents/core/gp_CadObjects.lsp` — helpery AutoCAD/VLAX, punkty, teksty, nearest text.
- `Contents/core/gp_PikietaFactory.lsp` — tworzenie bloku, kontekst, batch insert pikiet.
- `Contents/core/gp_PikietaStyle.lsp` — konwersje Blok/Tekst i update istniejacych pikiet.

## Realnie wydzielone moduly UI

- `Contents/ui/gp_SetupDialog.lsp` — podstawowe helpery UI oraz `c:GEO_SETUP`.
- `Contents/ui/gp_SetupAutosave.lsp` — autosave/walidacja `txt_h`, `z_prec`, stylu, widocznosci i koloru.
- `Contents/ui/gp_SetupPrefix.lsp` — prefixy numeracji w `GEO_SETUP`.
- `Contents/ui/gp_SetupGroup.lsp` — ladowanie grupy, tworzenie grupy, dialog nowego prefixu.
- `Contents/ui/gp_SetupMain.lsp` — helpery glownego okna oraz pelne `geocad-setup-show-main-dialog`.

## Celowo jeszcze nie robione

- Cleanup `gp_CoreLegacy.lsp` — jeszcze nie robiony.
- Optymalizacja konwersji Blok/Tekst — jeszcze nie robiona.
- Radar tekstow z eksportu — jeszcze nie przeniesiony.

## Zasada dalszych zmian

1. Najpierw shadow split bez zmiany logiki.
2. Potem test AutoCAD.
3. Dopiero potem cleanup legacy.
4. Dopiero po cleanupie optymalizacje `ssget` i konwersji.
