module Utils (module Utils) where

lastMaybe :: [a] -> Maybe a
lastMaybe []   = Nothing
lastMaybe list = Just (last list)

inRangeOf :: Ord a => a -> (a, a) -> Bool
inRangeOf val (from, to) = (from <= val) && (val <= to)

clamp :: Ord a => a -> a -> a -> a
clamp mn mx = max mn . min mx

splitHalves :: [a] -> ([a], [a])
splitHalves list = splitAt ((length list + 1) `div` 2) list

dupe :: a -> (a, a)
dupe a = (a, a)

fromIntegralPair :: (Integral a, Fractional b) => (a, a) -> (b, b)
fromIntegralPair (a, b) = (fromIntegral a, fromIntegral b)

-- | Approaches the target value with the given speed
approach :: Float -> Float -> Float -> Float
approach current target speed =
  if current < target
    then min (current + speed) target
    else max (current - speed) target

-- | Non-linear approach function
approachSmooth :: Float -> Float -> Float -> Float
approachSmooth current target speed = result
  where
    eps = 1
    val = current + (target - current) * speed

    result =
      if (val <= target && val + eps >= target) ||
         (val >= target && val - eps <= target)
        then target
        else val
