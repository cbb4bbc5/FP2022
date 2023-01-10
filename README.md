# FP2022

## Punktacja

[Punktacja](points.md)

## Linki do Hack.md

- [lista 2](https://hackmd.io/n3PDESuQRqerXNGPm6ZEuQ)
- [lista 3](https://hackmd.io/ubxCRB5UROqu1ZCUn5vYCw)
- [lista 4](https://hackmd.io/WMlK86sRTZKdaoojgTHEcA)
- [lista 5](https://hackmd.io/itbyQUFKSo68b1vOqdeseA)
- [Lista6](https://hackmd.io/BzBNxwE_TZOUkU_E0EbWug)
- [Lista 7](https://hackmd.io/_IVrUws1QRekAJEyCy9LCg)
- [lista 8](https://hackmd.io/dcAvZXhiQJ6cnJGUj5ZXEQ)

## Link do spotkania

[Google Meet](https://meet.google.com/zse-gjvv-nin)

## Jak wyglada deklaracja?

- Klonujemy repozytorium (jesli jeszcze tego nie zrobiliśmy)
- Tworzymy nowego brancha np `git checkout -b 666666/lista_02`
- Tworzymy nowy folder w ktorym umiescimy rozwiazania np `mkdir -p 666666/lista_02`
- Gotowe rozwiazania umieszczamy w **osobnych katalogach** ktorych nazwy to odpowiednie nr zadan np `1`, `2` itd
- **UWAGA** utworzenie folderu z numerem i umieszczenie w nim czegokolwiek jest rownoważne z deklaracją tego zadania.
- Gdy zacommitujemy juz zmiany pushujemy brancha na githuba np. `git add * && git commit -m "commit message" && git push origin 666666/lista_02`
- Przechodzimy do zakladki "Pull requests" i tworzymy nowy PR odpowiadajacy naszej branchowej deklaracji

## Branch

Wasze branche powinny miec nazwe pasujaca do wzorca `<nr indeksu>/lista_<nr listy>`

## Prezentacja
 ### Tematy dodatkowych prezentacji
 + ~~Functor/Applicative/Monad~~ (MS)
 + ~~Monoid + other algebraic typeclasses (Purescript)~~ (DB)
    - [haskell wiki](https://en.wikibooks.org/wiki/Haskell/Monoids)
    - [Functional Programming Made Easier](https://libgen.li/edition.php?id=140734530)
 + ~~Foldable/Traversable/Alternative~~ (MM)
    - [haskell wiki](https://en.wikibooks.org/wiki/Haskell/Foldable)
    - [Functional Programming Made Easier](https://libgen.li/edition.php?id=140734530)
+ (TK) konstrukcje ADT
    - wyklady/blog/ksiazka B.Milewskiego

+ (TK) Kleisli category
+ ~~__Monad transformers!!!__~~ ultra wazny temat (MW)
    - [Haskell wiki](https://en.wikibooks.org/wiki/Haskell/Monad_transformers)
    - [Functional Programming Made Easier](https://libgen.li/edition.php?id=140734530)
+ Template Haskell czyli makra w haskellu
    - [haskell wiki](https://wiki.haskell.org/Template_Haskell)
+ ~~Elm programming language - krotki wstep do tego jezyka + omowienie jak obchodzimy "nieczyste" obliczenia w Elmie~~ (DB)
+ ~~Higher Kinds (bardzo ciekawy temat) jak zrobic "typy zalezne" w haskellu~~ (KG)
    - Sandy Maguire - Thinking with Types
+ ~~Typy egzystencjalne~~ (KJ)
+ ~~Strzalki (arrows)~~ (JP)
    - [paper](http://www.cse.chalmers.se/~rjmh/afp-arrows.pdf)
+ Freer Monad alternatywa do transformatorow monad
    - [okmij](https://okmij.org/ftp/Haskell/extensible/)
    - [okmij paper](https://okmij.org/ftp/Haskell/extensible/more.pdf)
+ Soczewki i inne optyki (Lens) - Raczej trudny temat
