instance (Json a) => JSON [a] where
    toJValue = udefined    
    fromJValue = undefined

instance (Json a) => JSON [(String,a)] where
    toJValue = udefined    
    fromJValue = undefined