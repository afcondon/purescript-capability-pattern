module App.Layer.Four where -- Layers 4 & 3 common to Production and Test

-- Layer 4
-- strong types & pure, total functions on those types
newtype Name = Name String

getName :: Name -> String
getName (Name s) = s
