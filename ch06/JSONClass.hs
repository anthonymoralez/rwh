-- NOTE: you must add -i../ch05 to the ghc(i) command-line for this to work 
{-# LANGUAGE TypeSynonymInstances #-}
module JSONClass where 

import SimpleJSON

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where 
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _ = Left "not a JSON boolean"

instance JSON String where
    toJValue               = JString
    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue   = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue   = JNumber
    fromJValue = doubleToJValue id
