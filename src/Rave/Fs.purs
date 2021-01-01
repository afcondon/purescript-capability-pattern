module Rave.FS where

import Prelude (class Monad, Unit, pure, unit)
import Rave.RowTypes (type (+))

import Control.Monad.Except.Checked (ExceptV)
import Data.Variant (SProxy(..), inj, Variant)
import Node.Path (FilePath)


-- we wish to export this checked-exception wrapper for some underlying FS operation
writeEV
  ∷ ∀ r m.
    MonadFs m ⇒ 
    FilePath
  → String
  → ExceptV (FsError + r) m Unit
writeEV filePath string = pure unit




-- fake file system monad for demonstration purposes
class (Monad m) <= MonadFs m




-- Typed exceptions for node.FS
type FsPermissionDenied r = (fsPermissionDenied ∷ Unit | r)
type FsFileNotFound r     = (fsFileNotFound ∷ FilePath | r)

fsPermissionDenied ∷ ∀ r. Variant (FsPermissionDenied + r)
fsPermissionDenied = inj (SProxy ∷ SProxy "fsPermissionDenied") unit

fsFileNotFound ∷ ∀ r. FilePath → Variant (FsFileNotFound + r)
fsFileNotFound = inj (SProxy ∷ SProxy "fsFileNotFound")

type FsError r =
  ( FsPermissionDenied
  + FsFileNotFound
  + r
  )
