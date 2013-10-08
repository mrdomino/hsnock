> module Nock5K where

1 Structures

  A noun is an atom or a cell.  An atom is any natural number.
  A cell is an ordered pair of nouns.

> data Noun = Atom Integer | Cell Noun Noun deriving (Eq)


2 Reductions

  nock(a)           *a

> nock = tar

  [a b c]           [a [b c]]

> infixr 1 `Cell`


  ?[a b]            0

> wut (Cell a b) = Atom 0

  ?a                1

> wut a = Atom 1

  +[a b]            +[a b]

> lus (Cell a b) = error "+[a b]"

  +a                1 + a

> lus (Atom a) = Atom (1 + a)

  =[a a]            0

> tis (Cell a ap) | a == ap = Atom 0

  =[a b]            1

> tis (Cell a b) = Atom 1

  =a                =a

> tis a = error "=a"


  /[1 a]            a

> fas (Cell (Atom 1) a) = a

  /[2 a b]          a

> fas (Cell (Atom 2) (Cell a b)) = a

  /[3 a b]          b

> fas (Cell (Atom 3) (Cell a b)) = b

  /[(a + a) b]      /[2 /[a b]]

> fas (Cell (Atom a) b) | a > 2 && a `mod` 2 == 0 =
>   fas $ Atom 2 `Cell` (fas $ Atom (a `div` 2) `Cell` b)

  /[(a + a + 1) b]  /[3 /[a b]]

> fas (Cell (Atom a) b) | a > 3 && a `mod` 2 == 1 =
>   fas $ Atom 3 `Cell` (fas $ Atom (a `div` 2) `Cell` b)

  /a                /a

> fas a = error "/a"


  *[a [b c] d]      [*[a b c] *[a d]]

> tar (Cell a (Cell (Cell b c) d)) =
>   tar (a `Cell` b `Cell` c) `Cell` tar (a `Cell` d)


  *[a 0 b]          /[b a]

> tar (Cell a (Cell (Atom 0) b)) = fas $ b `Cell` a

  *[a 1 b]          b

> tar (Cell a (Cell (Atom 1) b)) = b

  *[a 2 b c]        *[*[a b] *[a c]]

> tar (Cell a (Cell (Atom 2) (Cell b c))) =
>   tar $ (tar (Cell a b)) `Cell` (tar (Cell a c))

  *[a 3 b]          ?*[a b]

> tar (Cell a (Cell (Atom 3) b)) = (wut.tar) (Cell a b)

  *[a 4 b]          +*[a b]

> tar (Cell a (Cell (Atom 4) b)) = (lus.tar) (Cell a b)

  *[a 5 b]          =*[a b]

> tar (Cell a (Cell (Atom 5) b)) = (tis.tar) (Cell a b)


  *[a 6 b c d]      *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]

> tar (Cell a (Cell (Atom 6) (Cell b (Cell c d)))) =
>   tar (a `Cell` Atom 2 `Cell` (Atom 0 `Cell` Atom 1) `Cell`
>        Atom 2 `Cell` (Atom 1 `Cell` c `Cell` d) `Cell`
>        (Atom 1 `Cell` Atom 0) `Cell` (Atom 2) `Cell`
>        (Atom 1 `Cell` Atom 2 `Cell` Atom 3) `Cell`
>        (Atom 1 `Cell` Atom 0) `Cell` Atom 4 `Cell` Atom 4 `Cell` b)

  *[a 7 b c]        *[a 2 b 1 c]

> tar (Cell a (Cell (Atom 7) (Cell b c))) =
>   tar $ a `Cell` Atom 2 `Cell` b `Cell` Atom 1 `Cell` c

  *[a 8 b c]        *[a 7 [[7 [0 1] b] 0 1] c]

> tar (Cell a (Cell (Atom 8) (Cell b c))) =
>   tar (a `Cell` Atom 7 `Cell`
>        ((Atom 7 `Cell` (Atom 0 `Cell` Atom 1) `Cell` b) `Cell`
>         Atom 0 `Cell` Atom 1) `Cell` c)

  *[a 9 b c]        *[a 7 c 2 [0 1] 0 b]

> tar (Cell a (Cell (Atom 9) (Cell b c))) =
>   tar (a `Cell` Atom 7 `Cell` c `Cell` Atom 2 `Cell`
>        (Atom 0 `Cell` Atom 1) `Cell` Atom 0 `Cell` b)

  *[a 10 [b c] d]   *[a 8 c 7 [0 3] d]

> tar (Cell a (Cell (Atom 10) (Cell (Cell b c) d))) =
>   tar (a `Cell` Atom 8 `Cell` c `Cell` Atom 7 `Cell`
>        (Atom 0 `Cell` Atom 3) `Cell` d)

  *[a 10 b c]       *[a c]

> tar (Cell a (Cell (Atom 10) (Cell b c))) = tar $ a `Cell` c


  *a                *a

> tar a = error "*a"
