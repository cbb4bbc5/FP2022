# Programowanie funkcyjne - projekt
### Autor
Kamil Galik, nr indeksu: 314570

### Idea
W ramach projektu powstanie implementacja biblioteki do typowanych wyrażeń regularnych. Biblioteka pozwoli na parsowanie wyrażeń regularnych i wyszukiwanie wzorca w zadanym tekście. W implementacji wykorzystane będą "typy zależne" w Haskellu, które uzyskam przy użyciu mechanizmu Higher Kinds.

Biblioteka posiadać będzie funkcje dla następujących operacji:
- Utworzenie wyrażenia regularnego poprzez konstruktory typów lub parsowanie String'ów. Być może typ regex'a będzie zawierał informacje o matching groups.
- Wyszukanie wzorca zadanego wyrażeniem regularnym w tekście przy użyciu backtrackingu (monada BT) z możliwością określenia typu zwracanego przez metodę wyszukiwania. Przykładowo zrzutowanie na Bool da odpowiedź True/False
- Możliwość wyszukiwania z opcją case-sensitive

### Spis użytych technologii i narzędzi
- Język programowania: *Haskell*
- Kompilator: *GHC*
- Narzędzie do zarządzania projektem: *Stack*

### Spis źródeł
- Sandy Maguire - "Thinking with Types"
