module ConnorCommon.Tuples 
  ( fst3, snd3, thr3
  , fst4, snd4, thr4, frth4) where

-- Threes
fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thr3 :: (a, b, c) -> c
thr3 (_, _, c) = c

-- Fours
fst4 :: (a, b, c, d) -> a
fst4 (a, _, _, _) = a

snd4 :: (a, b, c, d) -> b
snd4 (_, b, _, _) = b

thr4 :: (a, b, c, d) -> c
thr4 (_, _, c, _) = c

frth4 :: (a, b, c, d) -> d
frth4 (_, _, _, d) = d
