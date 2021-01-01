module Rave.HTTP where

import Prelude (class Monad, Unit, pure, unit)
import Rave.RowTypes (type (+))

import Control.Monad.Except.Checked (ExceptV)
import Data.Variant (SProxy(..), Variant, inj)


-- we wish to export this checked-exception wrapper for some underlying http operation
getEV
  ∷ ∀ r m.
    MonadHttp m ⇒ 
    String
  → ExceptV (HttpError + r) m String
getEV url = pure "dummy"





-- fake HTTP monad for demonstration purposes
class (Monad m) <= MonadHttp m




-- Typed exceptions for HTTP
type HttpServerError r = (httpServerError ∷ String | r)
type HttpNotFound    r = (httpNotFound ∷ Unit | r)
type HttpOther       r = (httpOther ∷ { status ∷ Int, body ∷ String } | r)

httpServerError ∷ ∀ r. String → Variant (HttpServerError + r)
httpServerError = inj (SProxy ∷ SProxy "httpServerError")

httpNotFound ∷ ∀ r. Variant (HttpNotFound + r)
httpNotFound = inj (SProxy ∷ SProxy "httpNotFound") unit

httpOther ∷ ∀ r. Int → String → Variant (HttpOther + r)
httpOther status body = inj (SProxy ∷ SProxy "httpOther") { status, body }

type HttpError r =
  ( HttpServerError
  + HttpNotFound
  + HttpOther
  + r
  )

