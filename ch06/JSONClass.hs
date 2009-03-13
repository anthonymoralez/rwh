-- NOTE: you must add -i../ch05 to the ghc(i) command-line for this to work 
import SimpleJSON

type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where 
    toJValue = id
    fromJValue = Right
