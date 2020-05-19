module Data.Function.Combinator.Birds
  ( bluebird
  , blackbird
  , bunting
  , becard
  , cardinal
  , dove
  , dickcissel
  , dovekies
  , eagle
  , baldEagle
  , finch
  , goldFinch
  , hummingbird
  , idiot
  , jay
  , kestrel
  , lark
  , mockingbird
  , doubleMockingbird
  , owl
  , queerbird
  , quixoticbird
  , quizzicalbird
  , quirkybird
  , quackybird
  , robin
  , starling
  , thrush
  , turing
  , vireo
  , warbler
  , converseWarbler
  , whybird
  
  , idiot'
  , warbler'
  , cardinal'
  , robin'
  , finch'
  , vireo'
  , idiot''
  , warbler''
  , cardinal''
  , robin''
  , finch''
  , vireo''

  , kite
  , omega
  , konstantMocker
  , crossedKonstantMocker
  , theta
  ) where

import Unsafe.Coerce (unsafeCoerce)

-- | B
bluebird :: (b -> c) -> (a -> b) -> a -> c
bluebird f g x = f (g x)

-- | B1
blackbird :: (c -> d) -> (a -> b -> c) -> a -> b -> d
blackbird f g x y = f (g x y)

-- | B2
bunting :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
bunting f g x y z = f (g x y z)

-- | B3
becard :: (c -> d) -> (b -> c) -> (a -> b) -> a -> d
becard f g h x = f (g ( h x))

-- | C
cardinal :: (a -> b -> c) -> b -> a -> c
cardinal f x y = f y x

-- | D
dove :: (a -> c -> d) -> a -> (b -> c) -> b -> d
dove f x g y = f x (g y)

-- | D1
dickcissel :: (a -> b -> c -> d) -> a -> b -> (e -> c) -> e -> d
dickcissel f x y g z = f x y (g z)

-- | D2
dovekies :: (a -> b -> c) -> (d -> a) -> d -> (e -> b) -> e -> c
dovekies f g x h y = f (g x) (h y)

-- | E
eagle :: (a -> b -> c) -> a -> (d -> e -> b) -> d -> e -> c
eagle f x g y z = f x (g y z)

-- | E^
baldEagle :: (a -> b -> c) -> (d -> e -> a) -> d -> e -> (f -> g -> b) -> f -> g -> c
baldEagle f g x y h v w = f (g x y) (h v w)

-- | F
finch :: b -> a -> (a -> b -> c) -> c
finch x y f = f y x

-- | G
goldFinch :: (a -> b -> c) -> (d -> b) -> d -> a -> c
goldFinch f g x y = f y (g x)

-- | H
hummingbird :: (a -> b -> a -> c) -> a -> b -> c
hummingbird f x y = f x y x

-- | I
idiot :: a -> a
idiot x = x

-- | J
jay :: (a -> b -> b) -> a -> a -> b -> b
jay f x y z = f x (f y z)

-- | K
kestrel :: a -> b -> a
kestrel x _ = x

-- | L
lark :: (b -> c) -> (a -> b) -> c
lark f g = f (g g') where g' = unsafeCoerce g

-- | M
mockingbird :: (a -> b) -> b
mockingbird x = x x' where x' = unsafeCoerce x

-- | M2
doubleMockingbird :: (a -> b -> c) -> a -> c
doubleMockingbird f x = mockingbird (f x)

-- | O
owl :: ((a -> b) -> a) -> (a -> b) -> b
owl f g = g (f g)

-- | Q
queerbird :: (a -> b) -> (b -> c) -> a -> c
queerbird f g x = g (f x)

-- | Q1
quixoticbird :: (a -> b) -> c -> (c -> a) -> b
quixoticbird f x g = f (g x)

-- | Q2
quizzicalbird :: a -> (b -> c) -> (a -> b) -> c
quizzicalbird x f g = f (g x)

-- | Q3
quirkybird :: (a -> b) -> a -> (b -> c) -> c
quirkybird f x g = g (f x)

-- | Q4
quackybird :: a -> (a -> b) -> (b -> c) -> c
quackybird x f g = g (f x)

-- | R
robin :: b -> (a -> b -> c) -> a -> c
robin x f y = f y x

-- | S
starling :: (a -> b -> c) -> (a -> b) -> a -> c
starling f g x = f x (g x)

-- | T
thrush :: a -> (a -> b) -> b
thrush x f = f x

-- | U
turing :: (a -> (b -> c) -> b) -> (b -> c) -> c
turing f g = g (f f' g) where f' = unsafeCoerce f

-- | V
vireo :: a -> b -> (a -> b -> c) -> c
vireo x y f = f x y

-- | W
warbler :: (a -> a -> b) -> a -> b
warbler f x = f x x

-- | W1
converseWarbler :: a -> (a -> a -> b) -> b
converseWarbler = cardinal warbler

-- | Y
whybird :: (x -> x) -> x
whybird x = x (whybird x)

-- | I*
idiot' :: ((a -> b) -> a) -> (a -> b) -> a
idiot' f g = f g

-- | W*
warbler' :: (a -> b -> b -> c) -> a -> b -> c
warbler' f x y = f x y y

-- | C*
cardinal' :: (a -> b -> c -> d) -> a -> c -> b -> d
cardinal' f x y z = f x z y

-- | R*
robin' :: (a -> b -> c -> d) -> c -> a -> b -> d
robin' f x y z = f y z x

-- | F*
finch' :: (a -> b -> c -> d) -> c -> b -> a -> d
finch' f x y z = f z y x

-- | V*
vireo' :: (a -> b -> c -> d) -> b -> a -> c -> d
vireo' f x y z = f y x z

-- | I**
idiot'' :: (a -> b -> c) -> a -> b -> c
idiot'' f g h =  f g h

-- | W**
warbler'' :: (a -> b -> c -> c -> d) -> a -> b -> c -> d
warbler'' f x y z = f x y z z

-- | C**
cardinal'' :: (a -> b -> c -> d -> e) -> a -> b -> d -> c -> e
cardinal'' f x y z w = f x y w z

-- | R**
robin'' :: (a -> b -> c -> d -> e) -> a -> d -> b -> c -> e
robin'' f x y z w = f x z w y

-- | F**
finch'' :: (a -> b -> c -> d -> e) -> a -> d -> c -> b -> e
finch'' f x y z w = f x w z y

-- | V**
vireo'' :: (a -> b -> c -> d -> e) -> a -> c -> d -> b -> e
vireo'' f x y z w = f x w y z

-- | KI
kite :: a -> b -> b
kite _ y = y

-- | Omega
--   The omega bird looks as mockingbird mockingbird.
--   But, I've implemented this as itself, because omega isn't going to stop anyway.
omega :: a
omega = omega -- mockingbird mockingbird

-- | KM
konstantMocker :: c -> (a -> b) -> b
konstantMocker = kestrel mockingbird

-- | C(KM)
crossedKonstantMocker :: (a -> b) -> c -> b
crossedKonstantMocker = cardinal konstantMocker

-- | Theta
theta :: (a -> a) -> a
theta = whybird owl
