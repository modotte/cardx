module Cardx.Util (maybeHead, defaultIfEmpty) where

import Relude

maybeHead :: [a] -> Maybe a
maybeHead = maybeAt 0

defaultIfEmpty :: a -> [a] -> a
defaultIfEmpty x xs = fromMaybe x $ maybeHead xs