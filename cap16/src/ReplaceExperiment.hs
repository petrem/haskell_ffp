module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Noris", Nothing, Just "Banatoris"]


replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

replaceLiftedTwice :: (Functor f, Functor g) => f (g a) -> f (g Char)
replaceLiftedTwice = (fmap . fmap) replaceWithP

replaceLiftedTwice' :: [Maybe [Char]] -> [Maybe Char]
replaceLiftedTwice' = replaceLiftedTwice

replaceLiftedThrice :: (Functor f1, Functor f2, Functor f3) => f1 (f2 (f3 a)) -> f1 (f2 (f3 Char))
replaceLiftedThrice = (fmap . fmap . fmap) replaceWithP

replaceLiftedThrice' :: [Maybe [Char]] -> [Maybe [Char]]
replaceLiftedThrice' = replaceLiftedThrice

printReplaced :: IO ()
printReplaced = do
  putStr "replaceWithP' lms:   " -- 'p'
  print (replaceWithP' lms)
  putStr "liftedReplace lms:   " -- "ppp"
  print (liftedReplace lms)
  putStr "liftedReplace' lms: "  -- "ppp"
  print (liftedReplace' lms)
  putStr "replaceLiftedTwice lms:     "  -- [Just 'p', Nothing, Just 'p']
  print (replaceLiftedTwice lms)
  putStr "replaceLiftedTwice' lms:    "  -- [Just 'p', Nothing, Just 'p']
  print (replaceLiftedTwice' lms)
  putStr "replaceLiftedThrice lms:    " -- [Just "ppppp", Nothing, Just "ppppppppp"]
  print (replaceLiftedThrice lms)
  putStr "replaceLiftedThrice' lms:   " -- [Just "ppppp", Nothing, Just "ppppppppp"]
  print (replaceLiftedThrice' lms)

