module Outrun.Data.Custom (module Outrun.Data.Custom) where

data WithCustom a b = Common a | Custom b

fromCustom :: (a -> b) -> WithCustom a b -> b
fromCustom func val = case val of
  Common common -> func common
  Custom custom -> custom
