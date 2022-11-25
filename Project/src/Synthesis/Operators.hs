{-# Language GADTs #-}


module Abstract.Machine
    ( Algorithm
    , Operation(..))
    ) where


import Data.Bits
import Data.BitVector
import Data.List.NonEmpty


newtype Algorithm a = Algorithm (NonEmpty (Word, Operation a))


data Operation a where

    -- Arithmetic Operators
    ADD  :: Operation a -> Operation a -> Operation a
    SUB  :: Operation a -> Operation a -> Operation a
    MUL  :: Operation a -> Operation a -> Operation a

    -- Bit-wise Operators
    AND  :: Operation a -> Operation a -> Operation a
    OR   :: Operation a -> Operation a -> Operation a
    XOR  :: Operation a -> Operation a -> Operation a
    NEG  :: Operation a -> Operation a

    -- Bit-shift Operators
    -- Index X Value --> Output
    SFTL :: Operation a -> Operation a -> Operation a
    SFTR :: Operation a -> Operation a -> Operation a
    ROTL :: Operation a -> Operation a -> Operation a
    ROTR :: Operation a -> Operation a -> Operation a

    --
    LIT  :: a -> Operation a
    VAR  :: Word -> Operation a


deriving newtype  instance Data (Algorithm a)


deriving stock    instance Data (Operation a)


deriving newtype  instance Generic (Algorithm a)


deriving stock    instance Generic (Operation a)


deriving newtype  instance NFData (Algorithm a)


deriving anyclass instance NFData (Operation a)


deriving newtype  instance Semigroup (Algorithm a)


assignment :: Word -> Operation a -> Algorithm a
assignment = curry (Algorithm . pure)



{-

Binary Operators: BV -> BV -> BV

( + ) Addition       modulo \(\mathbb{Z}^{d}\)
( - ) Subtraction    modulo \(\mathbb{Z}^{d}\)
( * ) Multiplication modulo \(\mathbb{Z}^{d}\)
( / ) Division       modulo \(\mathbb{Z}^{d}\)
(.&.) Bitwise AND
(.|.) Bitwise OR
-}


{-

Unary Operatiors: BV -> BV

complement Bitwise NEG

-}


{-

Nullary Operators: BV

Hexadecimal literals \(\in \mathbb{Z}^{d}\)

-}
