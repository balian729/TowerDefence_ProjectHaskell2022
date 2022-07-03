module Objects where

import CodeWorld
import Data.Time.Clock
import qualified Data.Text as T
import CodeWorld.Sketches
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}



-- | Defines the initial board and game itself including money, when money is given
-- and when enemies are out
initBoard :: (Int, Int) -> Game
initBoard (n, m) = Game {
  board = replicate m (replicate n (Nothing, (0,0))),
  money = 0,
  enemies = [],
  towers = [],
  time = 0,
  currentTower = FrozenMag,
  moneyCooldown = [0, 10,0.01, 10, 10,0.01,0.01, 10, 10, 0.01, 0.01, 10, 10,
    10,0.01, 10, 10, 0.01, 0.01, 0.01, 10, 10,0.01,0.01, 10, 10, 10, 0.01, 0.01,
      0.01, 0.01, 0.01,0.01,0.01,0.01,0.01, 10, 10, 10, 10],
  waveNumber = 0,
  waveTimingList = [5,0.01,10,0.01,15,0.01,15,0.01,5,0.01,10,0.01,15,0.01,15,
    0.01,17,0.01,3,0.01,15,0.01,15,0.01,15,0.01],
  shootCooldown = 2,
  gameOver = True
  }

-- | Constants defining a width and a length of a map
widthMap :: Int
widthMap = 11
lengthMap :: Int
lengthMap = 5

------------------------------UNITS---------------------------------------------

-- | These are the pictures of a towers and enemies that a game uses

fireMagPicture :: Picture
fireMagPicture = reflected (pi / 2) (lettering (T.pack "\x1F9D9"))


frozenMagPicture :: Picture
frozenMagPicture = (lettering (T.pack "\x1F936"))


elfPicture :: Picture
elfPicture = translated 0.4 (-0.3) (scaled 0.6 0.6 (lettering (T.pack "\x1F3F9")))
  <> lettering (T.pack "\x1F9DD")


haskellPic :: Picture
haskellPic = translated (-0.25) 0
             (translated 0.25 0 (rotated (pi/5) (colored purple (solidRectangle 0.07 1))) <>
             translated 0.03 (-0.23) (rotated (-(pi/5)) (colored purple (solidRectangle 0.07 0.55))) <>
             translated (-0.2) (-0.23) (rotated (-(pi/5)) (colored purple (solidRectangle 0.07 0.55))) <>
             translated (-0.2) 0.23 (rotated (pi/5) (colored purple (solidRectangle 0.07 0.55))) <>
             translated 0.5 0.15 (colored purple (solidRectangle 0.5 0.07)) <>
             translated 0.57 (-0.05) (colored purple (solidRectangle 0.35 0.07)))


ghostPicture :: Picture
ghostPicture = reflected (pi / 2) (lettering (T.pack "\x1F47B"))


catPicture :: Picture
catPicture = translated 0 0.2(dilated 0.23 (sketchedCat))


batPicture :: Picture
batPicture = lettering (T.pack "\x1F987")


scorpPicture :: Picture
scorpPicture = rotated (3 * pi / 2)(lettering (T.pack "\x1F982"))


zombiePicture :: Picture
zombiePicture = lettering (T.pack "\x1F9DF")

-- | These are the pictures of bullets, there are four types of them:
-- fireBalls, frozenBalls, arrows and functions
fireBallPicture :: Picture
fireBallPicture = rotated (pi / 2) (scaled 0.35 0.35 (lettering (T.pack "\x1F525")))

frozenBallPicture :: Picture
frozenBallPicture = scaled 0.35 0.35 (colored white (lettering (T.pack "\x2744")))

arrowPicture :: Picture
arrowPicture = scaled 0.35 0.35 (colored white (lettering (T.pack "\x27B3")))

funcPicture :: Picture
funcPicture = scaled 0.35 0.35 (colored purple (lettering (T.pack "F")))

----------------------------------BACKGROUND------------------------------------

-- | Unites the whole background pictures 
background :: Game -> Picture
background game = basePic <> shopPic <> platform <> planetsPic <> starsPic <> space

-- | Picture of the space itself
space :: Picture
space = colored black (solidRectangle 100 100)

-- | Picture of a base a player needs to defend
-- The picture is a logo of IU
basePic :: Picture
basePic = translated (-1.8) 2 (
          translated (-0.1) 0 (colored white (solidRectangle 0.04 0.32)) <>
          translated (-0.2) 0 (colored white (solidRectangle 0.04 0.22)) <>
          translated (-0.3) 0 (colored white (solidRectangle 0.04 0.3)) <>
          translated (0) 0.02 (colored white (solidRectangle 0.04 0.23)) <>
          translated (0.1) 0.04 (colored white (solidRectangle 0.04 0.37)) <>
          translated (0.2) 0.1 (colored white (solidRectangle 0.04 0.14)) <>
          translated (-0.12) (-0.2) (colored white (solidRectangle 0.65 0.04)) <>
          translated (-0.43) 0.02 (colored white (solidRectangle 0.04 0.43)) <>
          translated (-0.32) (0.2) (rotated (-pi/16) (colored white (solidRectangle 0.27 0.04))) <>
          translated (0) (0.23) (rotated (pi/10) (colored white (solidRectangle 0.4 0.04))) <>
          translated (0.27) (0.17) (rotated (-pi/3) (colored white (solidRectangle 0.3 0.04))) <>
          translated (0.27) (-0.03) (rotated (pi/4) (colored white (solidRectangle 0.22 0.04))) <>
          translated (0.2) (-0.16) (colored white (solidRectangle 0.04 0.13)) <>
          colored green (solidRectangle 1.7 1.7))
-----------------------------------STARS----------------------------------------
-- | The stars there are rendered such way that there is a certain star pattern
-- which is repeaten throughout the whole space

-- | Renders stars from one coordinates to other
starsPic :: Picture
starsPic = renderStars (-50, -50) (50, 50)

-- | Puts a star to a given coordinates
starAt :: (Double, Double) -> Picture
starAt (x, y) = translated x y (colored yellow (solidCircle 0.1))

-- | The pattern of stars which is repeatedn
starPattern :: Picture
starPattern = translated 5 5 (starAt(-4, - 4.3) 
  <> starAt (-3.5, -0.8) <> starAt(-2.2, -3.1)
  <> starAt(3, - 2.5) <> starAt(-1.2, 4) <> starAt(2, -1)
  <> starAt(-0.7, 0.1) <> starAt(1.5, 3.3) <> starAt(3.3, 2.5)
  <> starAt(-4.2, 4.6) <> starAt(1.8, -2.8) <> starAt(-3.2, 2.5))
                           
-- | Renders a pattern in certain coordinates
renderPattern :: (Integer, Integer) -> Picture
renderPattern (x, y) = translated dx dy (starPattern)
  where
    dx = fromIntegral x
    dy = fromIntegral y

-- | Renders a row in a given y coordinates and from "from" to "to" in x coords
renderRow :: Integer -> (Integer, Integer) -> Picture
renderRow y (from, to)
  | from > to = blank
  | otherwise = renderPattern (from, y) <> renderRow y (from + 10, to)

-- | Renders stars from one coords to other
renderStars :: (Integer, Integer) -> (Integer, Integer) -> Picture
renderStars (fromX, fromY) (toX, toY)
  | fromY > toY = blank
  | otherwise
       = renderRow fromY (fromX, toX)
      <> renderStars (fromX, fromY + 10) (toX, toY)

--------------------------------------------------------------------------------

-- | Picture of the planet on the background
planetsPic :: Picture
planetsPic =  translated (-4) 11 (scaled 5.1 5 saturn)

-- | Planet picture
saturn :: Picture
saturn = lettering (T.pack "\x1FA90")

-- | Picture of the platform(which is moon)
platform :: Picture
platform = translated 1.5 (-23.5) (rotated (11 * pi / 16) (scaled 15 15 sketchedMoon))

-- | Uniting the pictures for the shop
shopPic :: Picture
shopPic = pricesPic <> geniePic <> phrasePic 

-- | Picture of a genie
geniePic :: Picture
geniePic = translated (-0.5) 11 $ dilated 1.3 $ lettering $ T.pack "\x1F9DE"

-- | Picture of a phrase that genie says
phrasePic :: Picture
phrasePic = dilated 0.7 (translated 4.7 17.5 (colored black 
  (lettering $ T.pack "Wanna buy something?")))
         <> translated 3.3 12.3 (colored white (scaled 3.3 1 (solidCircle 1.1)))
       
-- | Picture of the prices and shop layout
pricesPic :: Picture
pricesPic = dilated 0.7 (translated 1.3 12.7 (colored black 
                  (lettering $ T.pack "100"))) <>
            dilated 0.7 (translated 4.2 12.7 (colored black 
                  (lettering $ T.pack "150"))) <>
            dilated 0.7 (translated 7.1 12.7 (colored black 
                  (lettering $ T.pack "200"))) <>
            dilated 0.7 (translated 9.8 12.7 (colored black 
                  (lettering $ T.pack "400"))) <>
            dilated 0.2 (translated 8.5 45 sketchedCoin) <> 
            dilated 0.2 (translated 18.4 45 sketchedCoin) <> 
            dilated 0.2 (translated 28.7 45 sketchedCoin) <> 
            dilated 0.2 (translated 38 45 sketchedCoin) <> 
            translated 4 9.6 (colored brown (solidRectangle 7.9 2.5))

    
-- draw "Game over"    
endGame :: Picture
endGame = lettering (T.pack "Game over")


------------------------DATA-------------------------------
--There are 4 type of towers in our game
data TypeTower = FrozenMag | FireMag | Elf | Haskell deriving(Eq)

-- There are 4 type of byllets in our game
data TypeBullet = FrozenBall | FireBall | Arrow | Func
-- | New characterisics
-- Each tower has type, picture, health points, cost, line, position and 
-- own bullets
data Tower = Tower{
  typeTower :: TypeTower,
  picTower :: Picture,
  hpTower :: HP,
  attackSpeed :: AtackSpeed,
  cost :: Cost,
  lineTower :: Line,
  posTower :: Int,
  bullets :: [Bullet]
  }
  
-- Each bullet has type, speed, picture, position, line and damage.  
data Bullet = Bullet{
  typeBul :: TypeBullet,
  speedBul :: Speed,
  pictureBul :: Picture,
  posBullet :: Double,
  lineBullet :: Line,
  damageBullet :: Int
  }
  
-- data enemy. Each enemy has health points, damage points, speed,
-- currrent coordinates (line and position) and picture.
data Enemy = Enemy {
  hpEnemy :: HP,
  damageEnemy :: Damage,
  speed :: Speed,
  lineEnemy :: Line,
  posEnemy :: Double,
  picEnemy :: Picture
}

--ELAMAN
data Game = Game {
  board :: Board,
  money :: Cost,
  enemies :: [Enemy],
  towers :: [Tower],
  gameOver :: Bool,
  time :: Double,
  currentTower :: TypeTower,
  moneyCooldown :: [Double],
  waveNumber :: Int,
  waveTimingList :: [Double],
  shootCooldown :: Double
  }  


-- play money  
type Money = Int

--cost of tower
type Cost = Int

--health points of enemy or tower
type HP = Int

--damage of enemy or tower
type Damage = Int

--arack speed of enemy or tower
type AtackSpeed = Int

--speed of enemy or bullet
type Speed = Double

--line of enemy/tower/bullet
type Line = Int  

--cell 
type Cell = (Maybe Tower, (Int, Int))

--game board
type Board = [[Cell]]


-- This function builds tower. There are 4 types of tower...
buildTower :: (Int, Int) -> TypeTower -> Tower
buildTower (i,j) typeTower = case typeTower of 
  FireMag -> Tower {
  typeTower = FireMag,
  picTower = fireMagPicture,
  hpTower = 1000,
  attackSpeed = 1,
  cost = 100,
  lineTower = j,
  posTower = i,
  bullets = []
}

  FrozenMag -> Tower {
  typeTower = FrozenMag,
  picTower = frozenMagPicture,
  hpTower = 1000,
  attackSpeed = 1,
  cost = 150,
  bullets = [],
  lineTower = j,
  posTower = i
}
  Elf -> Tower {
  typeTower = Elf,
  picTower = elfPicture,
  hpTower = 1000,
  attackSpeed = 2,
  cost = 200,
  bullets = [],
  lineTower = j,
  posTower = i
}
  Haskell -> Tower {
  typeTower = Haskell,
  picTower = haskellPic,
  hpTower = 1000,
  attackSpeed = 1,
  cost = 400,
  bullets = [],
  lineTower = j,
  posTower = i
}


--Each type of enemy has unique characteristics. There are 4 types or enemies.
--Ghost is default enemy with common speed and damage. This enemy is not extemely dangerous,
--bat is a fast enemy, scorpion is slow and tanky enemy,
--zombie is an extremely strong and fast enough enemy. 
-- enemy ghost
ghost :: Line -> Enemy
ghost line = Enemy {
  hpEnemy = 50,
  damageEnemy = 1,
  speed = 0.01,
  lineEnemy = line,
  posEnemy = 10,
  picEnemy = ghostPicture
  }
 
-- enemy scorpion 
scorpion :: Line -> Enemy
scorpion line = Enemy {
  hpEnemy = 100,
  damageEnemy = 1,
  speed = 0.008,
  lineEnemy = line,
  posEnemy = 10,
  picEnemy = scorpPicture
  }
 
-- enemy bat  
bat :: Line -> Enemy
bat line = Enemy {
  hpEnemy = 50,
  damageEnemy = 1,
  speed = 0.016,
  lineEnemy = line,
  posEnemy = 10,
  picEnemy = batPicture
  }
 
-- enemy zombie 
zombie :: Line -> Enemy
zombie line = Enemy {
  hpEnemy = 150,
  damageEnemy = 1,
  speed = 0.015,
  lineEnemy = line,
  posEnemy = 10,
  picEnemy = zombiePicture
  }

-- enemy killer cat
killerCat :: Line -> Enemy
killerCat line = Enemy {
  hpEnemy = 10000,
  damageEnemy = 1,
  speed = 0.005,
  lineEnemy = line,
  posEnemy = 10,
  picEnemy = catPicture
  }

--bullet fire ball
fireBall :: (Double, Line) -> Bullet 
fireBall (pos, line) = Bullet {
  typeBul = FireBall,
  speedBul = 0.1,
  pictureBul = (fireBallPicture),
  posBullet = pos,
  lineBullet = line,
  damageBullet = 5
  }

--bullet frozen ball
frozenBall :: (Double, Line) -> Bullet 
frozenBall (pos, line) = Bullet {
  typeBul = FrozenBall,
  speedBul = 0.1,
  pictureBul = (frozenBallPicture),
  posBullet = pos,
  lineBullet = line,
  damageBullet = 10
  }  

--bullet arrow
arrow :: (Double, Line) -> Bullet 
arrow (pos, line) = Bullet {
  typeBul = Arrow,
  speedBul = 0.2,
  pictureBul = (arrowPicture),
  posBullet = pos,
  lineBullet = line,
  damageBullet = 15
  }  

--bullet func
func :: (Double, Line) -> Bullet 
func (pos, line) = Bullet {
  typeBul = Arrow,
  speedBul = 0.15,
  pictureBul = (funcPicture),
  posBullet = pos,
  lineBullet = line,
  damageBullet = 40
  }  
