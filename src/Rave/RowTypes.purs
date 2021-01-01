module Rave.RowTypes where

-- Checked exceptions set up
type RowApply (f :: # Type -> # Type) (a :: # Type) = f a

infixr 0 type RowApply as +
