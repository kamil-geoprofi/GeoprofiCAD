# GeoprofiCAD UI

Docelowe miejsce dla kodu UI/DCL, przede wszystkim `GEO_SETUP`.

Aktualnie `gp_SetupDialog.lsp` istnieje jako shell typu shadow split. Pelna implementacja `GEO_SETUP` nadal jest w `core/gp_CoreLegacy.lsp`, zeby nie ryzykowac znikniecia komendy podczas czystego podzialu.

Nastepny bezpieczny krok to przeniesienie funkcji `geocad-setup-*` oraz `c:GEO_SETUP` do `ui/gp_SetupDialog.lsp` bez zmiany logiki.
