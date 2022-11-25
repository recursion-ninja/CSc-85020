{-# Language DeriveAnyClass #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveGeneric #-}
{-# Language DerivingStrategies #-}
{-# Language GADTs #-}
{-# Language LambdaCase #-}
{-# Language QuantifiedConstraints #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}


module Abstract.Machine
    ( Algorithm
    , Operation(..)
    , assignment
    ) where


import Control.DeepSeq (NFData)
import Control.Monad.Free
import Data.Bits
import Data.Data (Data)
import Data.Function (fix)
import Data.IntMap.Strict (IntMap, (!?), singleton)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Ap(..))
import Data.Traversable
import GHC.Exts (IsList(..))
import GHC.Generics (Generic) 
import Numeric.Natural (Natural)
import Text.Show (Show(..))


newtype Algorithm a = Algorithm (NonEmpty (Word, Operation a))


newtype Interpretation op bv val = Interpretation (FreeF Operation bv val)


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
    LIT  :: Natural -> Operation a
    VAR  :: Word    -> Operation a


deriving stock    instance Data a => Data (Algorithm a)


deriving stock    instance Data a => Data (Operation a)


deriving stock    instance Generic (Algorithm a)


deriving stock    instance Generic (Operation a)


deriving newtype  instance NFData a => NFData (Algorithm a)


deriving anyclass instance NFData a => NFData (Operation a)


deriving newtype  instance Semigroup (Algorithm a)


instance Functor Operation where

    fmap f = \case
        ADD  x y -> ADD  (f <$> x) (f <$> y)
        SUB  x y -> SUB  (f <$> x) (f <$> y)
        MUL  x y -> MUL  (f <$> x) (f <$> y)
        AND  x y -> AND  (f <$> x) (f <$> y)
        OR   x y -> OR   (f <$> x) (f <$> y)
        XOR  x y -> XOR  (f <$> x) (f <$> y)
        SFTL x y -> SFTL (f <$> x) (f <$> y)
        SFTR x y -> SFTR (f <$> x) (f <$> y)
        ROTL x y -> ROTL (f <$> x) (f <$> y)
        ROTR x y -> ROTR (f <$> x) (f <$> y)
        NEG  x   -> NEG  (f <$> x)
        LIT  x   -> LIT  x
        VAR  ref -> VAR  ref


instance IsList (Algorithm a) where

    type Item (Algorithm a) = (Word, Operation a)

    fromList    []  = error "The algorithm must have at least one operation!"
    fromList (x:xs) = Algorithm $ x :| xs

    toList (Algorithm ops) = toList ops


assignment :: Word -> Operation a -> Algorithm a
assignment = curry (Algorithm . pure)


modularBitField :: (Bits b, Enum b, Num b) => Interpretation f b val
modularBitField = undefined

  
{-
modularBitField :: (Bits a, Enum a, Num a) => IntMap b -> Operation a -> b
modularBitField mapping =
    let f :: Bits a => Operation a -> a
        f = modularBitField mapping
    in  \case
            ADD  x y -> f x   +   f y
            SUB  x y -> f x   -   f y
            MUL  x y -> f x   *   f y
            AND  x y -> f x  .&.  f y
            OR   x y -> f x  .|.  f y
            XOR  x y -> f x `xor` f y
            SFTL x y -> f y  `shiftL` fromEnum (f x)
            SFTR x y -> f y  `shiftR` fromEnum (f x)
            ROTL x y -> f y `rotateL` fromEnum (f x)
            ROTR x y -> f y `rotateR` fromEnum (f x)
            NEG  x   -> complement $ f x
            LIT  x   -> toEnum $ fromIntegral x
            VAR  ref ->
                let fault = error $ "No mapping for " <> show ref <> " in evaluation!"
                in  fromMaybe fault $ mapping !? fromEnum ref
-}



evaluate :: (Bits a, Num a, Monad m) => b -> Interpretation m b a -> Algorithm a -> m b
evaluate seed transform =
    let -- state :: IntMap a
        state = singleton 0 seed
    in  undefined


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
