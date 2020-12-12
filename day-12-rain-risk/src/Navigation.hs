module Navigation where

import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Direction = North | East | South | West deriving (Enum, Eq, Show)
data Instruction = Move Direction Int -- moving to the direction by x
                 | Forward Int        -- going forward by x
                 | Turn Int           -- positive: turning right, negative: turning left
                 deriving (Show)
data Ship = Ship {
    ns       :: Int,       -- e.q. 17 = north 17, -6 = south 6
    ew       :: Int,       -- same thing here with west, 10 = east 10, -49 = west 49
    facing   :: Direction, -- faced direction (part 1)
    waypoint :: (Int, Int) -- waypoint (part 2)
} deriving (Show)

initialShip :: Ship
initialShip = Ship { facing = East, ns = 0, ew = 0, waypoint = (1, 10) }

parseInstructions :: FilePath -> IO [Instruction]
parseInstructions path = readFile path >>= (\i -> return (mapMaybe parseInstruction . lines $ i))

parseDirection :: Char -> Maybe Direction
parseDirection c = case c of
    'N' -> Just North
    'E' -> Just East
    'S' -> Just South
    'W' -> Just West
    _   -> Nothing

parseInstruction :: String -> Maybe Instruction
parseInstruction []     = Nothing
parseInstruction (x:xs) =
    (readMaybe xs :: Maybe Int) >>=
    (\n -> let nodir = case x of
                'L' -> return (Turn (-n))
                'R' -> return (Turn n)
                'F' -> return (Forward n)
                _   -> Nothing
            in maybe nodir (\d -> return (Move d n)) (parseDirection x))

-- part 1
execute :: Instruction -> Ship -> Ship
execute i s = case i of
    Move d n  -> case d of
        North -> s { ns = ns s + n }
        East  -> s { ew = ew s + n }
        South -> s { ns = ns s - n }
        West  -> s { ew = ew s - n }
    Forward n -> execute (Move (facing s) n) s
    Turn n    -> s { facing = toEnum $ abs $ (fromEnum (facing s) + n `div` 90) `mod` 4 }

-- part 2
execute' :: Instruction -> Ship -> Ship
execute' i s = case i of
    Move d n  -> case d of
        North -> s { waypoint = (fst (waypoint s) + n, snd (waypoint s)) }
        East  -> s { waypoint = (fst (waypoint s), snd (waypoint s) + n) }
        South -> s { waypoint = (fst (waypoint s) - n, snd (waypoint s)) }
        West  -> s { waypoint = (fst (waypoint s), snd (waypoint s) - n) }
    Forward n -> s { ns = ns s + n * fst (waypoint s), ew = ew s + n * snd (waypoint s) }
    Turn n    -> s { waypoint = turn90 (n `div` 90 `mod` 4) (waypoint s) }
                        where turn90 n c@(ns, ew)
                                | n < 0  = turn90 (4-n) c
                                | n == 0 = c
                                | n > 0  = turn90 (n-1) (-ew, ns)

executeAll :: (Instruction -> Ship -> Ship) -> Ship -> [Instruction] -> Ship
executeAll f = foldl (flip f)

manhattan :: Ship -> Int
manhattan s = abs (ns s) + abs (ew s)