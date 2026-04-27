# Analiza i Prognozowanie Sprzedaży Detalicznej (Tasmania)
**Autorzy:** Monika Kłosowska, Amanda Miśkiewicz

**Rok:** 2026

---

## Wstęp
Celem niniejszego projektu jest analiza szeregu czasowego dotyczącego obrotów w branży spożywczej na Tasmanii oraz budowa modeli prognozy krótko- i długoterminowej. Porównujemy modele klasyczne (ARIMA, ETS) z metodami uczenia maszynowego (Sieci Neuronowe).

---

## 1. Eksploracja danych i wizualizacja

W tej sekcji wczytujemy dane `aus_retail`, losujemy konkretny szereg czasowy dla Tasmanii (branża spożywcza) i badamy jego strukturę: trend, sezonowość oraz korelacje.

### 1.1 Przygotowanie danych i losowanie szeregu
---
Na początku wczytujemy niezbędne biblioteki oraz wybieramy jedną, konkretną serię danych do analizy.

```r
library(tseries)
library(fpp3)
library(tidyr)

# Wczytanie zbioru danych 
data(aus_retail)

# Losowanie konkretnego szeregu
set.seed(435229)  
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1))
```

### 1.2 Wykresy eksploracyjne
---

W tej części dokonujemy wizualizacji szeregu, aby zidentyfikować kluczowe wzorce, takie jak trend, sezonowość oraz korelacje.

### Wykres szeregu czasowego

```r
myseries |>
  autoplot(Turnover) +
  labs(
    title = "Wylosowany szereg",
    x = "Czas",
    y = "Turnover"
  )
```
![Wykres wylosowanego szeregu czasowego](images/wylosowany_szereg.png)

### Wykres sezonowy
```r
myseries |>
  gg_season(Turnover) +
  labs(
    title = "Sezonowość szeregu aus_retail",
    x = "Kwartał / miesiąc",
    y = "Turnover"
  )
```
![Wykres sezonowy](images/wykres_sezonowy.png)

#### Interpretacja wykresu sezonowego:

Z wykresu sezonowego widzimy, że obroty sprzedaży w Australii mają wyraźną sezonowość:
* **Wyższe obroty w okresie świątecznym:** Największe piki sprzedażowe przypadają na **grudzień**.
* **Stabilne wzrosty:** Zauważalne systematyczne zwiększenie obrotów w miesiącach **marzec-czerwiec**.
* **Sezonowość letnia:** Okresy letnie charakteryzują się wyraźnie niższymi obrotami w porównaniu do miesięcy zimowych.

### Wykres subseries

```r
myseries |>
  gg_subseries(Turnover) +
  labs(
    title = "Wykres subseries dla aus_retail",
    x = "Okres sezonowy",
    y = "Turnover"
  )
```
![Wykres subseries](images/wykres_subseries.png)

#### Interpretacja wykresu subseries:

Analiza wykresu podserii pozwala na wyciągnięcie następujących wniosków:
* **Zmiany w czasie:** Wykres pokazuje, jak obroty sprzedaży zmieniały się w każdym miesiącu na przestrzeni różnych lat.
* **Sezonowość:** Widać wyraźną sezonowość (regularne zmiany co roku) z najwyższymi obrotami w **grudniu**. Sugeruje to silny wpływ okresu świątecznego na wzrost sprzedaży detalicznej.
* **Cykliczność:** Regularny wzrost obrotów w różnych miesiącach (widoczny poprzez wznoszące się linie średnich) wskazuje na cykliczność oraz długoterminową powtarzalność wzorca wzrostowego.

### Wykres opóźnień (Lag plot)

```r
myseries |>
  gg_lag(Turnover, geom = "point") +
  labs(title = "Lag plot dla aus_retail")
```
![Wykres opóźnień](images/wykres_opoznien.png)

#### Interpretacja wykresu opóźnień:

* **Wpływ przeszłości:** Wykres opóźnień obrazuje relację między historycznymi wartościami obrotów (`Turnover`) a ich obecnymi odczytami.
* **Charakter korelacji:** Punkty na wykresie układają się w wyraźną, rosnącą linię, co świadczy o **silnej korelacji dodatniej** między przeszłymi a obecnymi wartościami. 
* **Wniosek:** Taka struktura danych potwierdza, że obecne obroty są ściśle uzależnione od wyników z poprzednich miesięcy, co jest typowe dla szeregów z silnym trendem.

### 1.3 Analiza autokorelacji (ACF)
---

```r
myseries |>
  ACF(Turnover) |>
  autoplot() +
  labs(title = "ACF dla wylosowanego szeregu")
```
![ACF](images/acf.png)

#### Wnioski z wykresu ACF:

#### Kluczowe wnioski z ACF:
* **Silny trend:** Bardzo wysokie słupki, które powoli opadają, potwierdzają wyraźny trend wzrostowy.
* **Stabilność:** Szereg nie jest stabilny w czasie 
* **Sezonowość:** Kształt wykresu potwierdza występowanie cyklicznych, rocznych wzorców sprzedaży.

### 1.4 Dekompozycja STL
---

```r
fit_stl_retail <- myseries |>
  model(
    STL = STL(Turnover ~ trend(window = 13) + season(window = "periodic"), robust = TRUE)
  )

components(fit_stl_retail) |>
  autoplot() +
  labs(title = "Dekompozycja STL - aus_retail")
```
![Dekompozycja STL](images/dekompozycja_stl.png)

#### Wnioski z dekompozycji STL:

* **Trend:** Obroty w sprzedaży detalicznej wykazują stałą tendencję wzrostową w czasie.
* **Sezonowość:** Występują wyraźne zmiany cykliczne w ciągu roku, z charakterystycznymi szczytami sprzedaży w **grudniu**.
* **Reszty:** Istnieje zauważalna zmienność, której nie wyjaśnia trend ani sezonowość.

### 1.5 Wizualna ocena stabilności wariancji
---

```r
myseries |>
  autoplot(Turnover) +
  labs(title = "Ocena stabilności wariancji - skala oryginalna")

myseries |>
  autoplot(log(Turnover)) +
  labs(title = "Ocena stabilności wariancji - skala logarytmiczna")
```
![Skala oryginalna](images/skala_oryginalna.png)

![Skala logarytmiczna](images/skala_logarytmiczna.png)

#### Ocena stabilności wariancji:

* **Wariancja na skali oryginalnej:** Wyraźnie widać, że wariancja rośnie wraz z upływem czasu. Im wyższa wartość obrotów (*Turnover*), tym większe stają się wahania w danych. 

* **Po transformacji logarytmicznej:** Na wykresie wykorzystującym skalę logarytmiczną wariancja jest znacznie **bardziej stabilna**. 

### 1.6 Transformacja Box-Cox
---
Aby precyzyjnie dobrać transformację stabilizującą wariancję, stosujemy metodę Guerrero, która pozwala wyznaczyć optymalny parametr $\lambda$ (lambda).

Obliczony parametr wynosi $\lambda = 0.0902$.

```r
# Dobór parametru Box-Cox metodą Guerrero
lambda_info <- myseries |>
  features(Turnover, features = guerrero)

lambda <- lambda_info$lambda_guerrero[1]
lambda

# Generowanie wykresu po transformacji
myseries |>
  mutate(Turnover_boxcox = box_cox(Turnover, lambda)) |>
  autoplot(Turnover_boxcox) +
  labs(
    title = paste("Szereg po transformacji Box-Cox, lambda =", round(lambda, 3)),
    x = "Czas",
    y = "Przetransformowany Turnover"
  )
```

![Wykres po transformacji Box-Cox](images/wykres_box_cox.png)

#### Wnioski z zastosowania transformacji:

* **Podobieństwo do logarytmu:** Wykres po transformacji Box-Cox przy wartości $\lambda = 0.09$ jest bardzo zbliżony do wykresu po transformacji logarytmicznej. Wynika to z faktu, że parametr $\lambda$ jest bliski $0$.
* **Efekt stabilizacji:** Zastosowana transformacja jest w tym przypadku odpowiednia, ponieważ skutecznie stabilizuje wariancję.