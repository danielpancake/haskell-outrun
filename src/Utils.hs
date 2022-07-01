module Utils (module Utils) where

data WithCustom a b = Common a | Custom b

fromCustom :: (a -> b) -> WithCustom a b -> b
fromCustom func val = case val of
  Common common -> func common
  Custom custom -> custom

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

---- | Interpolation functions |--------------------------------

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


-- ---- | All interpolation functions below (typed InterpolationFunc)
-- ---- | take a value between 0 and 1
-- type InterpolationFunc = Float -> Float

-- easeInOutSine :: InterpolationFunc
-- easeInOutSine x = (1 - cos (x * pi)) / 2

-- easeInSine :: InterpolationFunc
-- easeInSine x = 1 - cos (x * pi / 2)

-- easeOutQuint :: InterpolationFunc
-- easeOutQuint x = 1 - ((1 - x) ** 5)
