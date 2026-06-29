# GeoprofiCAD UI

Docelowe miejsce dla kodu UI/DCL, przede wszystkim `GEO_SETUP`.

Na tym etapie UI zostaje jeszcze w `core/gp_CoreRuntime.lsp`, żeby pierwszy refaktor nie zmieniał zachowania. Następny bezpieczny krok to przeniesienie funkcji `geocad-setup-*` oraz `c:GEO_SETUP` do `ui/gp_SetupDialog.lsp`.
