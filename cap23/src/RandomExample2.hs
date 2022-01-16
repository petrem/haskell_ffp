module RandomExample2 where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random
import RandomExample

{-
1. randomR (1, 6) :: (...,RandomGen g) => g -> (a, g)
   in the context of functions monad
   return (intToDie n, s) = const (intToDie n, s)
2. do-notation desugars to:
    randomR (1, 6) >>= \(n, s) -> return (intToDie n, s)
3. This can be further written as
    randomR (1, 6) >>= \(n, s) -> const (intToDie n, s)
                       :: a               a' -> b -> a'
                       :: a -> b -> a'
    randomR (1, 6) >>= \(n, s) -> const (intToDie n, s)
    randomR (1, 6) >>= \(n, s) _ -> (intToDie n, s)

    .. using one possible implementation for monad (m >>= k = flip k <*> m):
    \_ (n, s) -> (intToDie n, s) <*> randomR (1, 6)

    .. and implementation for applicative (f <*> a = \r -> f r (a r))
    \r -> (\_ (n, s) -> (intToDie n, s)) r (randomR (1, 6) r)
    .. substitute r for g (to stand in for RandomGen)
    \g -> \(n, s) -> (intToDie n, s) (randomR (1, 6) g)
-}
rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes':: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie rollDie rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= 20 = count
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (count + 1) nextGen


rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit = go 0 0
  where
    go :: Int -> Int -> StdGen -> Int
    go sum count gen
      | sum >= limit = count
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (count + 1) nextGen


rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged limit = go 0 0 []
  where
    go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
    go sum count trail gen
      | sum >= limit = (count, trail)
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
              in go (sum + die) (count + 1) (intToDie die:trail) nextGen
