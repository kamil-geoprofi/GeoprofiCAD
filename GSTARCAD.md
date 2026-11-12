# GeoprofiCAD w GstarCAD — instalacja testowa

Ta gałąź dodaje osobny loader AutoLISP dla GstarCAD. **Nie zmienia** istniejącego pakietu AutoCAD `PackageContents.xml`.

## Instalacja

1. Pobierz całe repozytorium. Nie zmieniaj układu podfolderów `Contents/core` i `Contents/ui`.
2. W GstarCAD przejdź do `OPTIONS` → `Files` i dodaj pełną ścieżkę do folderu `GeoprofiCAD/Contents` w **Support File Search Path**.
3. Dodaj tę samą lokalizację do **Trusted Locations** (jeśli dostępne i wymagane przez ustawienia bezpieczeństwa programu). Używaj tylko zaufanego kodu.
4. Uruchom `APPLOAD` i wskaż plik `Contents/GeoprofiCAD_GstarCAD.lsp`.
5. Sprawdź najpierw `GEO_SETUP`, `WSTAW_PIKIETE`, `IMPORT_POINTS_V3_7`, następnie narzędzia niwelacyjne.
6. Jeśli wszystko działa, dodaj loader w `APPLOAD` → **Startup Suite**, aby ładował się wraz z sesją / rysunkiem, zgodnie z działaniem używanej wersji GstarCAD.

**Uwaga:** nie uruchamiaj loadera AutoCAD `.bundle` jako metody instalacji w GstarCAD. Ta ścieżka obsługiwana jest osobno. Plik `GeoprofiCAD.cuix` należy ewentualnie spróbować wczytać oddzielnie przez `CUILOAD` (zależnie od wersji GstarCAD). Loader nie próbuje automatycznie ładować interfejsu, ponieważ zgodność CUIX należy potwierdzić w aplikacji.

## Wymagania / ograniczenia

- Loader sprawdza obecność plików i ładuje istniejące moduły w kolejności zależności. Nie kompiluje ani nie konwertuje kodu.
- Istniejące polecenia nie zostały przepisane. **Brak dotychczas testów uruchomieniowych GstarCAD**.
- Kod używa Visual LISP / ActiveX, m.in. `vlax-curve-*`, `vla-InsertBlock`, `vlax-ldata-get/put`, dynamicznych DCL oraz funkcji plikowych. Poszczególne zachowania trzeba przetestować w docelowej wersji GstarCAD.
- W szczególności przetestuj zapis konfiguracji do DWG: w `core/gp_ProjectMemory.lsp` błędy `vlax-ldata-*` są przechwytywane, a funkcja zapisująca zwraca wartość nawet po nieudanym zapisie. Zapis i odczyt sprawdź po ponownym otwarciu rysunku.
- `gp_Import.lsp` i `gp_Export.lsp` używają PowerShell / Windows Forms do wyboru pliku; sprawdź ten przepływ na komputerze docelowym.
- Przed testami modyfikującymi rysunek zrób kopię DWG.

## Szybka lista testów

| Test | Co sprawdzić |
| --- | --- |
| `GEO_SETUP` | Okno DCL otwiera się; ustawienia zachowują się po zapisie, zamknięciu i ponownym otwarciu DWG |
| `WSTAW_PIKIETE` | Punkt, wysokość, numeracja, warstwy i blok / tekst |
| `IMPORT_POINTS_V3_7` | Wybór pliku, układ współrzędnych, separator, tworzenie pikiet |
| `EKSPORT_PIKIET_V22` | Wybór pliku i zgodność eksportowanych współrzędnych |
| `NIWELACJA_OSI` | Odległość po polilinii i wysokości |
| `NIWELACJA_MULTI` | `vlax-curve-*`, wybór obiektów i działanie geometrii |
| `SIATKA_PUNKTOW` | Generowanie i współrzędne punktów |
| `CUILOAD` | Czy `GeoprofiCAD.cuix` jest akceptowany oraz czy przyciski wykonują polecenia |

## Diagnostyka

Jeśli GstarCAD zgłasza `Nie znaleziono pliku gp_Core.lsp`, sprawdź czy folder `Contents` dodano do **Support File Search Path**. Sam `APPLOAD` nie gwarantuje, że kolejne wywołania `(load "gp_Core.lsp")` znajdą plik.

Jeśli loader zgłosi brak modułu, pobierz wszystkie pliki z repozytorium. Jeśli zgłosi błąd wykonywania określonej funkcji, zapisz dokładny komunikat, wersję GstarCAD i komendę, która go wywołała.
