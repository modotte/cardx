{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Cardx.Constant qualified as CC
import Cardx.Model (Card (CWild))
import Cardx.WildCard (WildCard (..))
import Cardx.WildKind (WildKind)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Relude

makeWilds :: WildKind -> Vector Card
makeWilds x = V.replicate 4 (CWild (WildCard {kind = x, score = CC.wildScore}))

main :: IO ()
main = putStrLn "Hello"
