install.packages("fpp3")
install.packages("tseries")

library(tseries)
library(fpp3)

######################################## 1.

data(aus_retail)

## 1. Losowanie szeregu
set.seed(435229)  
myseries <- aus_retail |>
  filter(`Series ID` == sample(aus_retail$`Series ID`, 1)) # Losujemy jedną serię danych

myseries

## 2. Wykresy eksploracyjne

# Wykres szeregu czasowego
myseries |>
  autoplot(Turnover) +
  labs(
    title = "Wylosowany szereg",
    x = "Czas",
    y = "Turnover" # obroty ze sprzedaży
  )

# Wykres sezonowy
myseries |>
  gg_season(Turnover) +
  labs(
    title = "Sezonowość szeregu aus_retail",
    x = "Kwartał / miesiąc",
    y = "Turnover"
  )

# Z wykresu sezonowości widzimy, że obroty sprzedaży w Australii mają wyraźną sezonowość:
# Wyższe obroty w okresie świątecznym (grudzień),
# Stabilne wzrosty w marcu-czerwcu.
# Okresy letnie mają niższe obroty w porównaniu do zimowych.

# Wykres subseries
myseries |>
  gg_subseries(Turnover) +
  labs(
    title = "Wykres subseries dla aus_retail",
    x = "Okres sezonowy",
    y = "Turnover"
  )

# Wykres subseries pokazuje, jak obroty sprzedaży zmieniały się w każdym miesiącu w różnych latach.
# Widać wyraźną sezonowość (regularne zmiany co roku), z wyższymi obrotami w grudniu, co sugeruje, że sprzedaż detaliczna
# wzrasta w okresie świątecznym.
# Regularny wzrost obrotów w różnych miesiącach wskazuje na cykliczność (długoterminowa powtarzalność)

# Wykres opóźnień (lag plot)
myseries |>
  gg_lag(Turnover, geom = "point") +
  labs(title = "Lag plot dla aus_retail")

# Wykres opóźnień pokazuje, jak przeszłe wartości obrotów (Turnover)
# wpływają na obecne wartości w szeregu czasowym.
# Punkty tworzą linię rosnącą, oznacza to silną korelację między przeszłymi a obecnymi wartościami.

## 3. ACF

# Funkcja autokorelacji
myseries |>
  ACF(Turnover) |>
  autoplot() +
  labs(title = "ACF dla wylosowanego szeregu")

# Oś X (lag): Pokazuje liczbę miesięcy, o które przesuwamy dane (np. lag 1 oznacza przesunięcie o jeden miesiąc)
# Oś Y (acf): Pokazuje autokorelację, czyli stopień powiązania między danymi w różnych miesiącach.
# Wartość bliska 1 oznacza, że dane są mocno skorelowane z opóźnieniem (np. miesiąc po miesiącu).
# Wartość bliska 0 oznacza brak powiązania.

# ACF (Autocorrelation Function) mierzy, jak przesunięte wartości w szeregu (np. wartość w jednym miesiącu) są powiązane z wartościami w poprzednich miesiącach.
# Wysoka autokorelacja dla małych lagów sugeruje, że wartości w danym miesiącu mają wpływ na przyszłe miesiące.
# Spadek autokorelacji wskazuje na spadający wpływ przeszłych danych na przyszłość.

## 4. Dekompozycja STL

fit_stl_retail <- myseries |>
  model(
    STL = STL(Turnover ~ trend(window = 13) + season(window = "periodic"), robust = TRUE)
  )

components(fit_stl_retail) |>
  autoplot() +
  labs(title = "Dekompozycja STL - aus_retail")


# Wnioski:
# - Trend: Obroty w sprzedaży detalicznej rosną w czasie.
# - Sezonowość: Obroty mają wyraźne zmiany w ciągu roku, z wyższymi wartościami w grudniu.
# - Reszta: Istnieje duża zmienność, która nie jest wyjaśniona przez trend ani sezonowość, co może sugerować wpływ czynników zewnętrznych.

# Czy dane wymagają transformacji?

# Wizualna ocena wariancji
myseries |>
  autoplot(Turnover) +
  labs(title = "Ocena stabilności wariancji - skala oryginalna")

myseries |>
  autoplot(log(Turnover)) +
  labs(title = "Ocena stabilności wariancji - skala logarytmiczna")

# Wariancja na skali oryginalnej: Widać, że wariancja rośnie z czasem 
# – im wyższa wartość obrotów (Turnover), tym większe wahania w danych. 
# To wskazuje na niestabilną wariancję, co może utrudniać analizę i modelowanie.

# Po transformacji logarytmicznej: Na wykresie po transformacji logarytmicznej wariancja 
# jest bardziej stabilna. Oznacza to, że dane stały się bardziej jednorodne, 
# co jest korzystne dla dalszej analizy.

## 5. Transformacja Box-Cox

# Dobór parametru Box-Cox metodą Guerrero
lambda_info <- myseries |>
  features(Turnover, features = guerrero)

lambda_info

# lambda ≈ 0 sugeruje, że transformacja logarytmiczna jest najlepsza.
# lambda ≈ 1 sugeruje, że nie potrzeba żadnej transformacji.
# lambda > 1 sugeruje potęgowanie danych.

# Stosujemy transformację Box-Cox na podstawie obliczonego parametru lambda, co powinno poprawić stabilność wariancji.

lambda <- lambda_info$lambda_guerrero[1]

# Wykres po transformacji

myseries |>
  mutate(Turnover_boxcox = box_cox(Turnover, lambda)) |>
  autoplot(Turnover_boxcox) +
  labs(
    title = paste("Szereg po transformacji Box-Cox, lambda =", round(lambda, 3)),
    x = "Czas",
    y = "Przetransformowany Turnover"
  )

# Wykres po transformacji Box-Cox przy lambda = 0.09 jest bardzo podobny do wykresu 
# po transformacji logarytmicznej, ponieważ lambda jest bliskie 0, co oznacza, że 
# transformacja Box-Cox w tym przypadku jest bardzo zbliżona do transformacji logarytmicznej.

# Dane wymagają transformacji, ponieważ wariancja rośnie wraz z poziomem szeregu, co wskazuje na niestabilność danych.
# Transformacja logarytmiczna (lub Box-Cox z lambda ≈ 0) jest odpowiednia, ponieważ stabilizuje wariancję, czyniąc dane bardziej jednorodnymi.


######################################## 2.

## 1. Tworzenie zbioru uczącego (przed 2011) i testowego
train <- myseries |>
  filter(Month < yearmonth("2011 Jan"))

test <- myseries |>
  filter(Month >= yearmonth("2011 Jan"))

## 2. Sprawdzenie stacjonarności

# Test ADF
# H0: Szereg jest NIESTACJONARNY
# H1: Szereg jest STACJONARNY

adf_result <- adf.test(train$Turnover)
adf_result
# p-value > 0.05, nie ma podstaw do odrzucenia H0. Szereg jest NIESTACJONARNY


# Test KPSS - dodatkowe sprawdzenie
# H0: Szereg jest STACJONARNY
# H1: Szereg jest NIESTACJONARNY
train |>
  features(Turnover, unitroot_kpss)
# p-value < 0.05, odrzucamy H0. Szereg jest NIESTACJONARNY


## 3 i 4. Budowa modeli porównawczych, aby sprawdzić co najlepiej poprawia wynik
fit <- train |>
  model(
    `Naiwna Sezonowa` = SNAIVE(Turnover),
    `Box-Cox + SNAIVE` = SNAIVE(box_cox(Turnover, lambda)), # lambda z zad poprzedniego
    `Dekompozycja STL` = decomposition_model(
      STL(Turnover ~ trend(window = 13) + season(window = "periodic"), robust = TRUE),
      SNAIVE(season_adjust) # prognozujemy dane odsezonowane metodą SNAIVE
    )
  )

# Generowanie prognoz na okres testowy
fc <- fit |> forecast(test)

# Wykres porównawczy
fc |>
  autoplot(myseries, level = NULL) +
  labs(title = "Porównanie metod prognozowania dla projektu",
       y = "Turnover", x = "Czas")

# Interpretacja wykresu
  # Wszystkie modele poprawnie odczytały sezonowość.
  # Dane rzeczywiste oznaczone czarną linią rosną znacznie szybciej niż prognozy.
  # Sama stabilizacja wariancji nie wystarczyła, by przewidzieć nagły skok obrotów po 2015 roku.

## 5. Ocena jakości prognoz (Accuracy)
fc_accuracy <- accuracy(fc, myseries)
fc_accuracy |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  arrange(RMSE)

# Wnioski:
  # 1. Najlepszym modelem okazał się Box-Cox + SNAIVE (najniższe wartości RMSE, MAE, MAPE).
  # 2. Transformacja Box-Cox poprawiła wynik w porównaniu do zwykłego SNAIVE.
    # Błąd MAPE spadł z 10.8% do 10.1%, co potwierdza, że stabilizacja wariancji 
    # pomogła w dopasowaniu modelu.
  # 3. Model Dekompozycja STL uzyskał identyczne wyniki jak Naiwna Sezonowa.
    # W tym przypadku dekompozycja nie wniosła dodatkowej poprawy prognozy.
  # 4. Wartość MASE > 1  dla wszystkich modeli potwierdza, że
    # modele te radzą sobie znacznie gorzej na zbiorze testowym niż uczącym (ze względu na zmianę trendu).
  
# Do analizy reszt w kolejnym punkcie wybieramy model "Box-Cox + SNAIVE".

# 6. Analiza reszt
best_model_name <- fc_accuracy$.model[which.min(fc_accuracy$RMSE)]

fit |>
  select(all_of(best_model_name)) |>
  gg_tsresiduals() +
  labs(title = paste("Analiza reszt dla modelu Box-Cox + SNAIVE:", best_model_name))

# Interpretacja: 
# 1. WYKRES CZASOWY RESZT:
# Reszty znajdują się głównie w wartościach dodatnic, wiec prognoza jest niedoszacowana.
# Rzeczywista sprzedaż rosła szybciej, niż przewidział to model.

# 2. WYKRES ACF (Autokorelacja):
# Dużó słupków wystają poza niebieskie linie przerywane, co oznacza, że
# reszty nie są białym szumem. W danych pozostały wzorce, których model nie wychwycił.

# 3. HISTOGRAM:
# Rozkład błędów jest przesunięty w prawo (średnia > 0).
# Prognozy są zbyt niskie względem rzeczywistości.

###
# Mimo że model Box-Cox + SNAIVE był najlepszy pod względem miar,
# analiza reszt pokazuje, że nie jest on modelem idealnym. 
# Należy rozważyć modele ARIMA lub ETS, które lepiej radzą sobie z autokorelacją reszt.

######################################## 3. Modele ETS

## 1. Dopasowanie modeli Holta-Wintersa 
fit_ets <- train |>
  model(
    `HW Multiplikatywny` = ETS(Turnover ~ error("M") + trend("A") + season("M")),
    `HW Tłumiony` = ETS(Turnover ~ error("M") + trend("Ad") + season("M")),
    `Model STL+ETS` = decomposition_model(
      STL(box_cox(Turnover, lambda) ~ trend(window = 13) + season(window = "periodic")),
      ETS(season_adjust ~ error("A") + trend("A") + season("N"))
    )
  )

## 2. Porównanie dokładności (ETS a Box-Cox + SNAIVE)
fc_ets <- fit_ets |> forecast(test)

fc_ets_accuracy <- fc_ets |> 
  accuracy(myseries) |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  arrange(RMSE)

fc_ets_accuracy

# Wnioski:
# Model HW Multiplikatywny znacząco poprawił wynik z poprzedniego zadania.
# RMSE spadło z 29.2 (SNAIVE) do 16.4.
# Ma błąd MAPE na poziomie 7.3%, więc to mało i najlepiej radzi sobie z rosnącą zmiennością sezonową szeregu.
# Model STL+ETS okazał się najgorszy.

## 3. Wybór najlepszego modelu ETS i analiza reszt
best_ets_name <- fc_ets_accuracy$.model[which.min(fc_ets_accuracy$RMSE)]

fit_ets |>
  select(all_of(best_ets_name)) |>
  gg_tsresiduals() +
  labs(title = paste("Analiza reszt dla modelu HW Multiplikatywnego:", best_ets_name))

# Interpretacja:
# 1. WYKRES CZASOWY RESZT:
# W przeciwieństwie do modelu SNAIVE, tutaj reszty oscylują wokół zera.
# Model przestał systematycznie niedoszacowywać prognozy.

# 2. AUTOKORELACJA (ACF):
# Słupki w ACF są znacznie niższe niż wcześniej, więc model ETS 
# znacznie lepiej sobie radzi. Nadal występują sezonowe 
# przekroczenia (np. luty, grudzień), ale błąd jest dużo mniejszy.

# 3. ROZKŁAD (Histogram):
# Przypomina rozkład normalny. Jest to duża poprawa względem 
# poprzedniego modelu, gdzie cały histogram był przesunięty w prawo.

###
# Model HW Multiplikatywny jest dużo lepszy niż SNAIVE
# Reszty są bliższe białemu szumowi niż w poprzednich testach.

## 4. Wykres prognoz ETS
fc_ets |>
  filter(.model == best_ets_name) |>
  autoplot(myseries, level = NULL) +
  labs(
    title = paste("Najlepsza prognoza metodą ETS:", best_ets_name),
    subtitle = "Porównanie z rzeczywistymi danymi po 2011 roku",
    x = "Czas", y = "Turnover"
  )

# Interpretacja:
# Prognoza idealnie podąża za linią danych rzeczywistych. 
# Model dobrze przewidział, że im wyższa sprzedaż, tym wyższe skoki w grudniu. 















