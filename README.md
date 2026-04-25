# GeoprofiCAD 🚀

Profesjonalna nakładka do programu AutoCAD stworzona z myślą o inżynierach drogowych i geodetach. Automatyzuje żmudne procesy obliczeniowe, interpolacje spadków oraz zarządzanie pikietażem na mapach numerycznych.

## 📦 Instalacja (Autoloading)

Nakładka wykorzystuje oficjalny standard Autodesk `.bundle`, co oznacza, że instaluje się w 100% automatycznie.

1. Pobierz repozytorium jako plik ZIP lub zrób `git clone`.
2. Upewnij się, że główny folder nazywa się `GeoprofiCAD.bundle`.
3. Skopiuj ten folder do ścieżki systemowej:
   `C:\ProgramData\Autodesk\ApplicationPlugins\`
4. Uruchom program AutoCAD. Nakładka załaduje się automatycznie.

## 🛠️ Moduły i Komendy

System opiera się na jednym centralnym "Mózgu" (silniku zarządzającym wyglądem) i szeregu specjalistycznych narzędzi roboczych.

### ⚙️ Ustawienia Globalne
* **`GEO_SETUP`** - Otwiera panel sterowania. Tutaj definiujesz globalny wygląd pikiet (inteligentne bloki lub punkty+tekst), wysokość czcionek, precyzję rzędnych, kolory oraz przedrostki warstw. Zmiany aplikują się do wszystkich kolejnych działań. Panel potrafi również błyskawicznie zaktualizować wygląd setek istniejących już pikiet na rysunku.

### 📥 Import / 📤 Eksport
* **`IMPORT_POINTS_V3_7`** - Inteligentny import współrzędnych z plików tekstowych/CSV z automatycznym rozpoznawaniem separatorów i układu kolumn.
* **`EKSPORT_PIKIET_V22`** - Zaawansowany eksport geometrii do pliku. Wykorzystuje "radar" do parowania punktów z opisami, automatycznie rozwiązuje konflikty duplikatów numeracji oraz posiada bezpiecznik tolerancji nakładania się punktów.

### 📐 Narzędzia Projektowe (Niwelacje)
* **`SPADKIPRO`** - Łańcuch spadków. Pozwala wyklikiwać kolejne punkty (np. dna rowu), dynamicznie podając spadek w procentach.
* **`NIWELACJA_OSI`** - Generuje pikiety wzdłuż wybranej osi (np. polilinii) co zadany interwał lub odległość, bazując na punkcie startowym i zadanym spadku podłużnym.
* **`NIWELACJA_KRAWEDZI`** - Klasyczne przenoszenie pikietażu. Rzutuje pikiety z osi na krawędź ze zdefiniowanym spadkiem poprzecznym jezdni. Posiada wektorowy bezpiecznik prostopadłości.
* **`NIWELACJA_2PKT`** - Interpolacja rzędnych pomiędzy dwoma wskazanymi punktami na trasie.
* **`WSTAW_PIKIETE`** - Narzędzie do szybkiego, ręcznego wstawiania punktów z inteligentnym licznikiem i pamięcią przedrostków (np. `woda_1`, `woda_2`).

---
*Stworzone dla szybkiej, precyzyjnej i bezbłędnej pracy w środowisku CAD.*