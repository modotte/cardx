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
import Cardx.Model qualified as CXM
import Cardx.WildCard (WildCard (..))
import Cardx.WildKind (WildKind)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Relude

main :: IO ()
main = putStrLn "Hello"
