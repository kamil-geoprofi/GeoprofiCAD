# GeoprofiCAD split plan

## Aktualna strategia

Refaktor idzie metoda `shadow split`:

1. `gp_CoreLegacy.lsp` nadal zawiera pelna stara implementacje.
2. Nowe pliki modulow sa ladowane po legacy.
3. Najpierw tworzymy strukture i mapujemy API.
4. Potem przenosimy definicje funkcji do modulow bez zmian logiki.
5. Dopiero po testach usuwamy przeniesione sekcje z legacy.

## Kolejnosc cleanupu legacy

1. ProjectMemory
2. Numbering
3. CadObjects
4. PikietaFactory
5. PikietaStyle
6. SetupDialog UI

## Test po kazdym wiekszym kroku

- start AutoCAD z bundle
- GEO_SETUP
- IMPORT_POINTS_V3_7
- WSTAW_PIKIETE
- konwersja Blok/Tekst na malym rysunku

## Czego nie robimy w tym etapie

- nie optymalizujemy ssget
- nie zmieniamy logiki konwersji
- nie przenosimy jeszcze radaru eksportu
- nie zmieniamy publicznych nazw funkcji
