{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}


module CodingTheory
  ( buildVector
  , buildMatrix
  , transpose
  , mul
  , bitStringsN
  , choose
  , allH
  , tryH
  , codewords
  , showSolution
  ) where

import Data.Maybe
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Foldable
import Data.List qualified as L
import Data.Tuple (swap)
import Data.Vector.Unboxed (Vector, (!), generate)
import Data.Vector.Unboxed qualified as V
import GHC.IsList (IsList(fromListN))

import Debug.Trace

data BitMatrix = BitMatrix Int Int (Vector Bool)
    deriving stock (Eq, Ord)


instance Show BitMatrix where

    show (BitMatrix 1 _ v) = "< " <> showRow v <> " >"
    show (BitMatrix _ 1 v) = "< " <> showRow v <> " >T"
    show bm =
        let f = ("| " <>) . (<> " |\n") . showRow
        in  ("\n" <>) . foldMap f . toRows $ bm


showRow :: Vector Bool -> String
showRow = L.intersperse ' ' . V.foldMap (\b -> if b then "1" else "0")


mul :: BitMatrix -> BitMatrix -> BitMatrix
--mul lhs@(BitMatrix m1 n1 _) rhs@(BitMatrix m2 n2 _) | trace (fold [ "dimensions (", show m1," x ", show n1, ") * (", show m2," x ",show n2,")" ]) False = undefined
mul lhs@(BitMatrix m1 n1 _) rhs@(BitMatrix m2 n2 _)
    | n1 /= m2 = error $ fold [ "dimensions (", show m1," x ", show n1, ") * (", show m2," x ",show n2,") mismatch: ", show n1, " != ", show m2 ]
    | otherwise =
        let lhsRows = toRows lhs
            rhsCols = toRows $ transpose rhs
            g i =
                let (row, col) = i `divMod` n2
                in  vAdd $ V.zipWith (&&) (lhsRows !! row) (rhsCols !! col) 
        in  BitMatrix m1 n2 $ generate (m1 * n2) g


vAdd :: Vector Bool -> Bool
vAdd = V.foldl' (/=) False


buildVector :: (Eq n, Foldable f, Num n) => f n -> BitMatrix
buildVector xs =
    let row = fmap getBit $ toList xs
        n   = length row
    in  BitMatrix 1 n $ fromListN n row


buildMatrix :: (Eq n, Foldable f, Foldable t, Num n) => f (t n) -> BitMatrix
buildMatrix xs =
    let rows = (fmap getBit . toList) <$> toList xs
        m    = length rows
    in  case invariantTransformation length rows of
            Nothing -> error "Not all rows have equal columns"
            Just n  -> BitMatrix m n . fromListN (m * n) $ fold rows


transpose :: BitMatrix -> BitMatrix
transpose bm@(BitMatrix m n _) =
    let g :: Int -> Bool
        g = (bm .!.) . swap . (`divMod` m)
    in  BitMatrix n m $ generate (m*n) g


(.!.) :: BitMatrix -> (Int, Int) -> Bool
(.!.) (BitMatrix m n v) (i,j)
    | i <  0 = error $ fold [ "i (", show (i,j), ") is below dimensional bounds: (", show m, " x ", show n, ")" ]
    | j <  0 = error $ fold [ "j (", show (i,j), ") is below dimensional bounds: (", show m, " x ", show n, ")" ]
    | i >= m = error $ fold [ "1 (", show (i,j), ") is above dimensional bounds: (", show m, " x ", show n, ")" ]
    | j >= n = error $ fold [ "j (", show (i,j), ") is above dimensional bounds: (", show m, " x ", show n, ")" ]
    | otherwise = v ! (i * n + j)


getBit :: (Eq n, Num n) => n -> Bool
getBit 0 = False
getBit _ = True


-- | O(m) Return the rows
toRows :: BitMatrix -> [Vector Bool]
toRows bm@(BitMatrix m n _) = 
    let takeRow :: Int -> Vector Bool
        takeRow i = generate n $ \j -> bm .!. (i,j)
    in  takeRow <$> [ 0 .. m - 1 ]


-- |
-- \( \mathcal{O} \left( n \right) \)
--
-- Applies a transformation to each element of the structure.
-- If /every/ application of the transformation yields the same result value
-- for each element of the structure then this function will return @Just v@
-- where @v@ is the invariant value across the transformation.
-- If the transformation does not produce an invariant value across the
-- structure, or the structure is empty, this function returns @Nothing@.
--
-- See 'equalityOf' if you want to discard the @Just@ value.
--
-- ==_Example==
--
-- >>> invariantTransformation (`mod` 10) [ 9, 19, 29, 39, 49 ]
-- Just 9
--
-- >>> invariantTransformation (`mod`  7) [ 9, 19, 29, 39, 49 ]
-- Nothing
invariantTransformation :: (Eq b, Foldable t) => (a -> b) -> t a -> Maybe b
invariantTransformation f xs =
  case toList xs of
    []   -> Nothing
    y:ys ->
      let v = f y
      in  if all (\e -> f e == v) ys
          then Just v
          else Nothing



bitStringsN :: Int -> [[Word]]
bitStringsN = fromAlphabetN [[0],[1]]


fromAlphabetN :: [[a]] -> Int -> [[a]]
fromAlphabetN alpha n = let f = ((++) <$> alpha <*>) in head . drop n $ iterate f [[]]


choose :: (Foldable f, Ord a) => f a -> Int -> Set (Set a)
choose n' k' =
    let as = Set.fromList $ toList n'

        op :: Ord a => Int -> (Set a, Set a) -> Set (Set a)
        op z (_, es) = (Set.singleton (Set.findMin es) <>) `Set.map` go (Set.deleteMin es) z
        
        go :: Ord a => Set a -> Int -> Set (Set a)
        go n k
            | k == 0 = Set.singleton mempty
            | otherwise =
                foldMap ( (op (k - 1)) . (`Set.splitAt` n) ) [ 0 .. length n - 1 ]
    in  go as k'


allH :: Set BitMatrix
allH = Set.fromList [ buildMatrix [ x, y, z ] | x <- bitStringsN 5, y <- bitStringsN 5, z <- bitStringsN 5]


tryH :: BitMatrix -> Map BitMatrix (Set BitMatrix)
tryH h =
    let syndrome x =
            let v  = buildVector x
                v' = transpose v
                s  = h `mul` v'
            in  s `seq` Map.insertWith (<>) s (Set.singleton v)
    in foldr syndrome mempty $ bitStringsN 5


codewords :: Set BitMatrix
codewords = Set.fromList $ buildVector <$> [[0,0,0,0,0],[0,1,0,1,0],[1,1,0,1,0],[1,0,0,0,0]] :: Set BitMatrix


check :: Set BitMatrix -> Maybe BitMatrix
check bvs
    | length bvs /= 4 = Nothing
    | otherwise =
        let addTo (BitMatrix _ _ v1) (BitMatrix m n v2) = BitMatrix m n $ V.zipWith (/=) v1 v2
            isLeader (as, bs) =
                let e   = Set.findMin bs
                    new = addTo e `Set.map` bvs
                in  if   new == codewords
                    then Just e
                    else Nothing
        in  asum $ isLeader . (`Set.splitAt` bvs) <$> [ 0 .. 4 ]



showSolution :: Map BitMatrix (Set BitMatrix) -> IO (Map BitMatrix ())
showSolution = Map.traverseWithKey (\k v -> putStr ("  " <> show k <> " -> ") *> putStr (show (fromJust (check v)) <> " : ") *> putStrLn (show v) )

--take 1 . Prelude.filter (all isJust . fmap check) . Prelude.filter (maybe False (== codewords) . Map.lookup (transpose (buildVector [0,0,0]))) . Prelude.filter ((==8) . length) $ tryH <$> Set.toList allH 
