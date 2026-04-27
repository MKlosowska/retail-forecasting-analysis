# install.packages("fpp3")
# install.packages("tseries")
# install.packages("tidyr")

library(tseries)
library(fpp3)
library(tidyr)

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

######################################## 4. Modele ARIMA

## 1. Automatyczny dobór modelu ARIMA
fit_arima <- train |>
  model(
    `ARIMA automatyczna` = ARIMA(Turnover)
  )

fit_arima |> report()

# Interpretacja - model: (2,1,2)(0,1,2)[12]

# RÓŻNICOWANIE (d=1, D=1): Model automatycznie zastosował różnicowanie 
# zwykłe i sezonowe. Dzięki temu dane stały się stacjonarne.

# STRUKTURA: Model jest bardzo złożony. Wykorzystuje 2 parametry AR (autokorelacja) 
# i 2 parametry MA (średnia ruchoma) zarówno dla części zwykłej, jak i sezonowej.

# DOPASOWANIE: sigma^2 = 8.171 sugeruje niską wariancję błędu.
# Niskie wartości s.e. (błędu standardowego) przy współczynnikach 
# świadczą o ich istotności statystycznej.

## 2. Sprawdzenie stacjonarności danych bazowych
# Wyciągamy informację o parametrach modelu w formie tabeli
fit_arima |> 
  tidy() |> 
  select(.model, term) |> 
  distinct()

# Interpretacja

# 1. AR1, AR2 (Autoregresja): Model uwzględnia korelacje z dwoma poprzednimi 
#    miesiącami

# 2. MA1, MA2 (Średnia ruchoma): Model koryguje prognozę na podstawie 
#    błędów z dwóch poprzednich okresów.

# 3. SMA1, SMA2 (Sezonowa średnia ruchoma): 
#    Model koryguje bieżącą prognozę sezonową, patrząc na błędy prognoz 
#    z tych samych miesięcy w poprzednich latach.

# Tabela potwierdza, że model jest w pełni sezonowy.

# Wizualna reszt 
fit_arima |> 
  select(`ARIMA automatyczna`) |> 
  gg_tsresiduals() +
  labs(title = "Analiza reszt modelu ARIMA - weryfikacja stacjonarności")

# Interpretacja:
# Reszty oscylują wokół zera, co potwierdza stacjonarność. Widać jednak, że rozrzut rośnie.
# Model ARIMA pozbył się autokorelacji znacznie lepiej niż SNAIVE, 
# Histogram błędów przypomina rozkład normalnyi jest wycentrowany na zerze, więc
# nie jest obciążony (nie myli się systematycznie w jedną stronę).
# Reszty są bliskie białemu szumowi.

## 3. Porównanie ARIMA z HW Multiplikatywnym
fc_arima <- fit_arima |> forecast(test)

accuracy_comparison <- bind_rows(
  fc_ets |> accuracy(myseries), 
  fc_arima |> accuracy(myseries)
) |> 
  select(.model, RMSE, MAE, MAPE, MASE) |> 
  arrange(RMSE)

accuracy_comparison

# Wniosek: ARIMA jest najlepszym modelem. Ma najniższe RMSE (15.8) oraz MAPE (7.12%).


######################################## 5. 

# 1. Budowa modeli
fit_all <- train |>
  model(
    `Naiwny` = NAIVE(Turnover),
    `S-Naiwny` = SNAIVE(Turnover),
    `ETS (Auto)` = ETS(Turnover),
    `ARIMA (Auto)` = ARIMA(Turnover),
    `Theta` = THETA(Turnover),
    `Regresja (Trend+Sezon)` = TSLM(Turnover ~ trend() + season()),
    `Sieć Neuronowa (NNAR)` = NNETAR(Turnover)
  )

# 2. Sprawdzenie braków - nie ma NULL, więc modele zostały zbudowane poprawnie.
fit_all 

# Statystyki dopasowania dla wszystkich modeli
glance(fit_all)


# ARIMA ma najniższy wskaźnik AIC (1653) oraz najwyższą wiarygodność (log_lik = -819).
# Model ETS ma najmniejszy błąd (sigma2), więc niemal idealnie dopasował się do danych historycznych.
# Modele Naiwne i Regresja mają wysoką wariancję błędu (sigma2), więc nie radzą sobie z szumem w danych tak dobrze jak ARIMA.
# ARIMA i ETS to najlepsze modele. 

# 3.Prognozy na test
fc_all <- fit_all |> forecast(test)

# 4. Porównanie
summary_metrics <- fc_all |> 
  accuracy(myseries) |>
  select(.model, RMSE, MAE, MAPE, MASE) |>
  mutate(MASE_pod_1 = MASE < 1) |>
  arrange(RMSE)

summary_metrics

# Interpretacja:

# 1. Najlepszym modelem okazał się model ARIMA (Auto) RMSE = 15.8. 
# Model ten najlepiej radzi sobie z jednoczesnym opanowaniem trendu 
# wzrostowego i silnej sezonowości sprzedaży żywności na Tasmanii.
# ARIMA uzyskała najniższy wynik MASE = 2.39. Jest ona ponad dwukrotnie lepsza od metody naiwnej.

# 2. Na drugim miejscu ETS (16.4), a na trzecim Theta (20.7).

# 3. Sieć Neuronowa (21.9) i Regresja (25.6) okazały się 
# znacznie gorsze. Sugeruje to, że proste algorytmy statystyczne 
# są w tym przypadku skuteczniejsze niż uczenie maszynowe.

# 5. Wizualne porównanie trzech najlepszych modeli z danymi rzeczywistymi)
top_3_models <- summary_metrics |> head(3) |> pull(.model)

fc_all |>
  filter(.model %in% top_3_models) |>
  autoplot(myseries |> filter_index("2005 Jan" ~ .), level = NULL) +
  labs(title = "Top 3 Modele vs Rzeczywista Sprzedaż",
       subtitle = "Porównanie prognoz na próbie testowej",
       y = "Turnover") +
  theme_minimal()

# Interpretacja

# 1. ARIMA i ETS: Oba modele niemal idealnie pokrywają się ze sobą. 
#    Bardzo dobrze wyłapały rosnącą amplitudę sezonowości. 
#    Dobrze przewidziały gwałtowny skok sprzedaży (2018).

# 2. THETA: Wyraźnie odstaje od reszty.

# Wykres potwierdza, że najlepszym modelem jest ARIMA.







