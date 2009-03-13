-- NOTE: you must add -i../ch05 to the ghc(i) command-line for this to work 
import SimpleJSON
import JSONClass 

instance (JSON a) => JSON [a] where
    toJValue = undefined
    fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined
