# Stakler: Zew Zapisów #

## Motywacja ##

Zdecydowałem się na ten temat projektu, gdyż chciałem zrobić coś podobnego
wcześniej, tylko prawdopodobnie w Pythonie. Teraz jednak pojawiła się okazja do
sprawdzenia jak język funkcyjny nada się do zadań, do których tradycyjnie
użyłbym Pythona + BeautifulSoup + requests.

Sprawne wyciąganie wszelkich dostępnych informacji w systemie zapisy znacznie
ułatwi mi planowanie następnego semestru, eliminując żmudne wpisywanie zapytań w
wyszukiwarkę w tamtym systemie i niejednokrotne wracanie, by zobaczyć, czy
rzeczywiście poprawnie zapamiętałem to czego szukałem.

## Opis działania ##

Program ma przede wszystkim działać w trybie tekstowym, umożliwiając
przyjmowanie parametrów w celu wykonania jednorazowego zapytania (lub serii
zapytań realizowanej przez np. skrypt).

Przykładowe użycie:

``` bash
stakler --name Andrzej --type student
```

zwróci listę indeksów (i być może imion) studentów o imieniu Andrzej.

Program ma również pozwalać na bardziej zaawansowane zapytania, łączące kilka
parametrów, jak bycie zapisanym na podany przedmiot, czy podany semestr. Jego
główną cechą ma jednak być zbieranie informacji o przedmiotach, takich jak:

  * prowadzący, 
  * liczba osób zapisanych do grupy
  * godziny trwania
  * liczba osób w kolejce
  * typ przedmiotu
  * liczba punktów ECTS
  * sala
  * opis (?)

Zapytanie:

``` bash
stakler --class "programowanie funkcyjne"
```

zwróci informacje widoczne na stronie przedmiotu w systemie zapisy. Precyzyjniej
można będzie np. podać dzień, by sprawdzić, czy wtedy jakaś grupa jest otwarta.

Końcowo program ma umożliwiać wygenerowanie raportu na temat aktualnego planu
studenta, tak jak plan w systemie zapisy, ale z dodatkowymi informacjami, takimi
jak liczba ECTSów lub typ przedmiotu. Zebrane w ten sposób dane mają ułatwić
podjęcie decyzji, czy wypisać się z danych zajęć, zmienić grupę itd.

Program nie ma na celu umożliwiania zapisów, będzie tylko w wygodny sposób
przedstawiał wszystkie dostępne możliwości zgodne z podanymi kryteriami.

## Technologie ##

Język programowania to będzie ocaml z modułami lambdasoup i cohttp (czy podobne,
tego jeszcze nie wiem). Testy również będą zawarte. Co do zaawansowanych tematów
to zobaczę, który najbardziej pasuje do mojej wizji.
