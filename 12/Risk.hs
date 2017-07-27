{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where
import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = do a <- aForces
              d <- dForces
              return $ calcOutcome a d b
  where
      forces = calcForces b
      aForces = calcMove $ attackers forces
      dForces = calcMove $ defenders forces

calcOutcome :: [DieValue] -> [DieValue] -> Battlefield -> Battlefield
calcOutcome a d (Battlefield at df) = result $ foldr calc (at, df) (map outcome $ zip a d)
  where
        result (x, y) = Battlefield x y
        calc p1 p2 = ((fst p1) + (fst p2), (snd p1) + (snd p2))
        outcome x
                | (fst x) > (snd x) = (0, -1)
                | otherwise = (-1, 0)


calcMove :: Army -> Rand StdGen [DieValue]
calcMove x = do dies <- replicateM x die
                return $ sortBy dCompare dies
  where dCompare d1 d2 = flip compare (unDV d1) (unDV d2)
         
calcForces :: Battlefield -> Battlefield
calcForces (Battlefield a d) = Battlefield {
                attackers = calcA a,
                defenders = calcD d }
  where
        calcA x
              | x <= 0 = 0 
              | otherwise = min (x - 1) 3
        calcD x
              | x <= 0 = 0
              | otherwise = min x 2

defendersFail :: Battlefield -> Bool
defendersFail b = defenders b <= 0

attackersFail :: Battlefield -> Bool
attackersFail b = attackers b < 2


invade :: Battlefield -> Rand StdGen Battlefield
invade b = do result <- battle b
              if (defendersFail result || attackersFail result)
                then return result
                else invade result

successProb :: Battlefield -> Rand StdGen Double
successProb b = do items <- (replicateM 1000 $ invade b)
                   let result = filter attackersFail items
                   return $ fromIntegral (length result) / 1000

successProb2 :: Battlefield -> Rand StdGen [Battlefield]
successProb2 b = do items <- (replicateM 1000 $ invade b)
                    return items

test = do
  value <- evalRandIO $ successProb (Battlefield 10 30)
  putStrLn $ show value

test2 = do
  value <- evalRandIO $ successProb2 (Battlefield 10 3)
  let strs = map show value
  putStrLn $ unlines strs
  let result = length $ filter attackersFail value
  putStrLn $ "Attackers fail times : " ++ (show result)
