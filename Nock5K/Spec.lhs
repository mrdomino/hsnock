
> module Nock5K.Spec (Noun (Atom, (:-)), nock) where

1 Structures

  A noun is an atom or a cell.  An atom is any natural number.
  A cell is an ordered pair of nouns.

> data Noun = Atom Integer | Noun :- Noun deriving (Eq)


2 Reductions

  nock(a)           *a

> nock :: Noun -> Noun
> nock = tar

  [a b c]           [a [b c]]

> infixr 1 :-


  ?[a b]            0

> wut (a :- b) = Atom 0

  ?a                1

> wut a = Atom 1

  +[a b]            +[a b]

> lus (a :- b) = error "+[a b]"

  +a                1 + a

> lus (Atom a) = Atom (1 + a)

  =[a a]            0

> tis (a :- a') | a == a' = Atom 0

  =[a b]            1

> tis (a :- b) = Atom 1

  =a                =a

> tis a = error "=a"


  /[1 a]            a

> fas (Atom 1 :- a) = a

  /[2 a b]          a

> fas (Atom 2 :- a :- b) = a

  /[3 a b]          b

> fas (Atom 3 :- a :- b) = b

  /[(a + a) b]      /[2 /[a b]]

> fas (Atom a :- b) | a > 2 && a `mod` 2 == 0 =
>   fas $ Atom 2 :- fas (Atom (a `div` 2) :- b)

  /[(a + a + 1) b]  /[3 /[a b]]

> fas (Atom a :- b) | a > 3 && a `mod` 2 == 1 =
>   fas $ Atom 3 :- fas (Atom (a `div` 2) :- b)

  /a                /a

> fas a = error "/a"


  *[a [b c] d]      [*[a b c] *[a d]]

> tar (a :- (b :- c) :- d) = tar (a :- b :- c) :- tar (a :- d)


  *[a 0 b]          /[b a]

> tar (a :- (Atom 0 :- b)) = fas $ b :- a

  *[a 1 b]          b

> tar (a :- (Atom 1 :- b)) = b

  *[a 2 b c]        *[*[a b] *[a c]]

> tar (a :- Atom 2 :- b :- c) = tar $ tar (a :- b) :- tar (a :- c)

  *[a 3 b]          ?*[a b]

> tar (a :- Atom 3 :- b) = (wut.tar) (a :- b)

  *[a 4 b]          +*[a b]

> tar (a :- Atom 4 :- b) = (lus.tar) (a :- b)

  *[a 5 b]          =*[a b]

> tar (a :- Atom 5 :- b) = (tis.tar) (a :- b)


  *[a 6 b c d]      *[a 2 [0 1] 2 [1 c d] [1 0] 2 [1 2 3] [1 0] 4 4 b]

> tar (a :- Atom 6 :- b :- c :- d) =
>   tar (a :- Atom 2 :- (Atom 0 :- Atom 1) :- Atom 2 :- (Atom 1 :- c :- d) :-
>        (Atom 1 :- Atom 0) :- Atom 2 :- (Atom 1 :- Atom 2 :- Atom 3) :-
>        (Atom 1 :- Atom 0) :- Atom 4 :- Atom 4 :- b)

  *[a 7 b c]        *[a 2 b 1 c]

> tar (a :- Atom 7 :- b :- c) = tar $ a :- Atom 2 :- b :- Atom 1 :- c

  *[a 8 b c]        *[a 7 [[7 [0 1] b] 0 1] c]

> tar (a :- Atom 8 :- b :- c) =
>   tar (a :- Atom 7 :- ((Atom 7 :- (Atom 0 :- Atom 1) :- b) :-
>         Atom 0 :- Atom 1) :- c)

  *[a 9 b c]        *[a 7 c 2 [0 1] 0 b]

> tar (a :- Atom 9 :- b :- c) =
>   tar (a :- Atom 7 :- c :- Atom 2 :- (Atom 0 :- Atom 1) :- Atom 0 :- b)

  *[a 10 [b c] d]   *[a 8 c 7 [0 3] d]

> tar (a :- Atom 10 :- (b :- c) :- d) =
>   tar (a :- Atom 8 :- c :- Atom 7 :- (Atom 0 :- Atom 3) :- d)

  *[a 10 b c]       *[a c]

> tar (a :- Atom 10 :- b :- c) = tar $ a :- c


  *a                *a

> tar a = error "*a"
