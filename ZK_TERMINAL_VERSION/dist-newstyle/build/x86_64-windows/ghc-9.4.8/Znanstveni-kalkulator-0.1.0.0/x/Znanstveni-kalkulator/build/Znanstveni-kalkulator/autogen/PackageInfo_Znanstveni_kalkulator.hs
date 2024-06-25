{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_Znanstveni_kalkulator (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "Znanstveni_kalkulator"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Znanstveni kalkulator"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
