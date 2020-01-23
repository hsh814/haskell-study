--values like "Hi", map have their own type: kind
--kind is type of type
--in ghci: :k

{-
    Prelude> :k Int
    Int :: *
What is *?: It means it's concrete type
    Prelude> :k Maybe
    Maybe :: * -> *
Maybe gets one concrete type and returns concrete type like Maybe Int
    Prelude> :k Maybe Int
    Maybe Int :: *
    Prelude> :k Either
    Either :: * -> * -> *
type constrcutor can be curried:
    Prelude> :k Either String
    Either String :: * -> *
    Prelude> :k Functor
    Functor :: (* -> *) -> Constraint
-}

