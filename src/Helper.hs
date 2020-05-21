module Helper
    ( unPack
    , isRight
    )
where


-- Take value out of Maybe.
unPack :: Maybe a -> a
unpack Nothing = 0
unPack (Just a) = a

isRight :: Either a b -> Bool
isRight (Left  _) = False
isRight (Right _) = True
