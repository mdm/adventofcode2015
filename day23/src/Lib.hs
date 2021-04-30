module Lib
    ( part1
    , part2
    ) where

import Text.Parsec.String (Parser)
import Text.Parsec (string, parse)
import Text.Parsec.Char (digit)
import Text.Parsec.Combinator (many1)
import Debug.Trace(trace)
import Data.Maybe (mapMaybe)


data State = State {
  hard :: Bool
, bossHp :: Int
, bossDamage :: Int
, playerHp :: Int
, manaAvailable :: Int
, manaSpent :: Int
, shield :: Int
, poison :: Int
, recharge :: Int
} deriving Show

makeState hard (hp, dmg) = State hard hp dmg 50 500 0 0 0 0

parseBoss :: Parser (Int, Int)
parseBoss = do
                string "Hit Points: "
                hp <- many1 digit
                string "\nDamage: "
                dmg <- many1 digit
                return (read hp, read dmg)


applyHardMode :: Bool -> State -> State
applyHardMode player state = if player && hard state then state { playerHp = playerHp state - 1 } else state

applyPoison :: State -> State
applyPoison state = if poison state > 0 then state { bossHp = bossHp state - 3 } else state

applyRecharge :: State -> State
applyRecharge state = if recharge state > 0 then state { manaAvailable = manaAvailable state + 101 } else state

applyDecrement :: State -> State
applyDecrement state = state { shield = max 0 (shield state - 1), poison = max 0 (poison state - 1), recharge = max 0 (recharge state - 1) }

applyEffects :: Bool -> State -> State
applyEffects player = applyDecrement . applyRecharge . applyPoison . applyHardMode player

canMagicMissile :: State -> Bool
canMagicMissile state = manaAvailable state >= 53

castMagicMissile :: State -> State
castMagicMissile state = state { manaAvailable = manaAvailable state - 53, manaSpent = manaSpent state + 53, bossHp = bossHp state - 4 }

canDrain :: State -> Bool
canDrain state = manaAvailable state >= 73

castDrain :: State -> State
castDrain state = state { manaAvailable = manaAvailable state - 73, manaSpent = manaSpent state + 73, bossHp = bossHp state - 2, playerHp = playerHp state + 2 }

canShield :: State -> Bool
canShield state = manaAvailable state >= 113 && shield state == 0

castShield :: State -> State
castShield state = state { manaAvailable = manaAvailable state - 113, manaSpent = manaSpent state + 113, shield = 6 }

canPoison :: State -> Bool
canPoison state = manaAvailable state >= 173 && poison state == 0

castPoison :: State -> State
castPoison state = state { manaAvailable = manaAvailable state - 173, manaSpent = manaSpent state + 173, poison = 6 }

canRecharge :: State -> Bool
canRecharge state = manaAvailable state >= 229 && recharge state == 0

castRecharge :: State -> State
castRecharge state = state { manaAvailable = manaAvailable state - 229, manaSpent = manaSpent state + 229, recharge = 5 }

spells :: [(State -> Bool, State -> State)]
-- spells = [(canMagicMissile, castMagicMissile), (canDrain, castDrain), (canShield, castShield), (canPoison, castPoison), (canRecharge, castRecharge)]
spells = [(canMagicMissile, castMagicMissile), (canDrain, castDrain), (canShield, castShield), (canPoison, castPoison)]

playerTurn :: State -> Maybe Int
playerTurn state | playerHp afterEffects <= 0 = Nothing
                 | null availableSpells = Nothing
                 | null futures = if canRecharge afterEffects then bossTurn $ castRecharge afterEffects else Nothing
                 | otherwise = {- trace ("P " ++ show afterEffects) -} Just $ minimum futures
    where afterEffects = applyEffects True state
          availableSpells = map snd . filter (\x -> fst x afterEffects) $ spells
          futures = mapMaybe (\s -> bossTurn $ s afterEffects) availableSpells

bossTurn :: State -> Maybe Int
bossTurn state | bossHp afterEffects <= 0 = Just $ manaSpent state
               | otherwise = {- trace ("B " ++ show afterEffects) -} playerTurn $ afterEffects { playerHp = newHp }
    where afterEffects = applyEffects False state
          newHp = if shield state > 0 then playerHp state - max 1 (bossDamage state - 7) else playerHp state - bossDamage state

part1 :: String -> String
part1 = show . maybe undefined id . playerTurn . makeState False . either undefined id . parse parseBoss ""

part2 :: String -> String
part2 = show . maybe undefined id . playerTurn . makeState True . either undefined id . parse parseBoss ""
