module Game where

import CodeWorld
import Data.Time.Clock
import qualified Data.Text as T
import CodeWorld.Sketches
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fdefer-typed-holes #-}


import      Objects

-- | updates the whole game
updateGame :: Double -> Game -> Game
updateGame dt game = game {
    time = dt + (time game),
    moneyCooldown = fst (updateMoney1 (moneyCooldown game) dt),
    waveTimingList = fst (updateWave (waveTimingList game) dt),
    waveNumber = if snd (updateWave (waveTimingList game) dt) then
      waveNumber game + 1
      else waveNumber game,
    money =  if snd (updateMoney1 (moneyCooldown game) dt)  then
      (money game) + 100
      else money game,
    enemies = currentWave,
    shootCooldown = fst (updateShoot (shootCooldown game) dt),
    towers = if snd (updateShoot (shootCooldown game) dt) then
      map (createBullet)(actualTowers) 
      else map (movingBullets) (actualTowers),
    gameOver = if (gameOver game) then
      correctState game
      else False
  }
  where 
  ---------------------------------------WAVES--------------------------------
    currentWave = case  (waveNumber game)  of
      0 -> map (movingEnemy) actualEnemies
      1 -> map (movingEnemy) (listEnemy1 ++ (actualEnemies))
      3 -> map (movingEnemy) (listEnemy2 ++ (actualEnemies))
      5 -> map (movingEnemy) (listEnemy3 ++ (actualEnemies))
      7 -> map (movingEnemy) (listEnemy4 ++ (actualEnemies))
      9 -> map (movingEnemy) (listEnemy5 ++ (actualEnemies))
      11 -> map (movingEnemy) (listEnemy6 ++ (actualEnemies))
      13 -> map (movingEnemy) (listEnemy7 ++ (actualEnemies))
      15 -> map (movingEnemy) (listEnemy8 ++ (actualEnemies))
      17 -> map (movingEnemy) (listEnemy9 ++ (actualEnemies))
      19 -> map (movingEnemy) (listEnemy10 ++ (actualEnemies))
      21 -> map (movingEnemy) (listEnemy11 ++ (actualEnemies))
      23 -> map (movingEnemy) (listEnemy12 ++ (actualEnemies))
      otherwise -> map (movingEnemy) actualEnemies
--NOT WAVES      

    enemiesOnScreen = takeWhile onScreenEnemy (enemies game)
--    actualEnemies = (filter (isLife) (hittingList hitting (enemies game) (towers game)))
    towersOnScreen = takeWhile isLifeTower (towers game)
    (actualEnemi, newTowers) = roundBul (enemiesOnScreen) (towersOnScreen)
    (actualEnemie, actualTower) = ((deleteTow actualEnemi newTowers))
    actualTowers = filter (isLifeTower) actualTower
    actualEnemies = filter (isLife) actualEnemie
-- Waves
    listEnemy1 = [ghost 2]
    listEnemy2 = [ghost 2, bat 1]
    listEnemy3 = [scorpion 2, ghost 1, ghost 3]
    listEnemy4 = [scorpion 0, ghost 1, bat 3, bat 2]
    listEnemy5 = [zombie 0, bat 1, ghost 2, bat 3, ghost 4]
    listEnemy6 = [ghost 0, scorpion 1, bat 2, scorpion 3, bat 4]
    listEnemy7 = [scorpion 0, bat 1, zombie 2, bat 3, bat 4]
    listEnemy8 = [scorpion 0, scorpion 1, scorpion 2, scorpion 3, scorpion 4]
    listEnemy9 = [zombie 0, bat 1, zombie 2, bat 3, ghost 4]
    listEnemy10 = [bat 0, zombie 1, bat 2, zombie 3, scorpion 4]
    listEnemy11 = [zombie 0, zombie 1, zombie 2, zombie 3, zombie 4]
    listEnemy12 = [killerCat 2]

-- | Updates the tower on the map
updateTowers :: [Tower] -> [Enemy] -> [Tower]
updateTowers towers enemies = actualTowers
  where
    actualEnemies = (filter (isLife) (hittingList hitting 
      (takeWhile onScreenEnemy enemies) (towers)))
    actualTowers = (filter (isLifeTower) (deleteTowers actualEnemies towersOnScreen))
    towersOnScreen = takeWhile isLifeTower (towers)

-- | Updates the wave of enemies, determining whether it is time to send new wave
-- or not
updateWave :: [Double] -> Double -> ([Double], Bool)
updateWave [] _ = ([], False)
updateWave (x:xs) dt 
  | x - dt > 0 = ((x - dt :xs), False) 
  | otherwise =  (smallUpdate(xs) (dt - x), True)
  where 
    smallUpdate (x:xs) dt = x - dt:xs 
    smallUpdate [] _= []

-- | Updates a money, determining whether it is time to give money or not
updateMoney1 :: [Double] -> Double -> ([Double], Bool)
updateMoney1 [] _ = ([], False)
updateMoney1 (x:xs) dt 
  | x - dt > 0 = ((x - dt :xs), False) 
  | otherwise = (smallUpdate(xs) (dt - x), True)
  where 
    smallUpdate (x:xs) dt = x - dt:xs 
    smallUpdate [] _= []

-- This function allows to towers to fire a shot every 2 seconds
updateShoot :: Double -> Double -> (Double, Bool)
updateShoot timer dt 
  | timer - dt > 0 = (timer - dt, False)
  | otherwise = (timer - dt + 2, True)


-- | Try update an element at a given position in a list.
updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ []     = []
updateAt 0 f (x:xs) = case f x of
   y  ->  y : xs
updateAt i f (x:xs) = case updateAt (i - 1) f xs of
   ys -> x : ys  

-- draw enemies
drawEnemy :: Enemy -> Picture
drawEnemy enemy = translated x y (picEnemy enemy)
  where 
    x = (posEnemy enemy)
    y = fromIntegral (lineEnemy enemy)


-- | Draw one board cell at given coordinates.
drawCellAt :: Int -> Int -> Cell -> Picture
drawCellAt i j (cell, _) 
  | (i + j) `mod` 2 == 0 = translated x y (colored grey(solidRectangle 1 1))
  | otherwise            = blank
    where
      x = fromIntegral i
      y = fromIntegral j


--drawing all bullet of all buildings
drawAllBullets :: Game -> Picture
drawAllBullets game = pictures (map (drawBullets) (map bullets (towers game)) )


--drawing all bullets that are on the screen
drawBullets :: [Bullet] -> Picture
drawBullets listBullet = pictures (map (drawBullet) 
  (takeWhile onScreenBullet listBullet))


--drawing one bullet
drawBullet :: Bullet -> Picture
drawBullet bullet = translated  (  (posBullet bullet)) 
  (fromIntegral (lineBullet bullet)) (pictureBul bullet)

-- | Draw a rectangular board.
drawBoard :: Game -> Picture
drawBoard game =  pictures (map pictures drawMap) 
  where
      drawMap = map drawRow (zip [0..] (board game))
      drawRow (k, row) = map  drawCell (zip [0..] row)
        where
          drawCell (l, cell) =   drawCellAt l k cell


-- | Draw current amount of money
drawMoney :: Game -> Picture
drawMoney game = translated 10 10 (lettering(T.pack 
  (show (money game))) <> translated 1.25 0.2 (dilated 0.3 sketchedCoin) 
    <> colored brown(solidRectangle 4 1) )


-- | Draw amount of time passed 
drawTime :: Game -> Picture
drawTime game = translated 10 7 (lettering(T.pack 
  (show (round(time game)))) <> 
  translated 1.3 0.1 (dilated 0.27 sketchedClock) <> 
  colored brown(solidRectangle 4 1))
  
  
-- | Draw enemies that are currently on screen
drawEnemies :: Game -> Picture
drawEnemies game = pictures (map (drawEnemy) 
  (takeWhile onScreenEnemy (enemies game)))


--this function checks if enemy is on the screen
onScreenEnemy :: Enemy -> Bool
onScreenEnemy enemy = (posEnemy enemy >= 0)


-- This function checks if bullet is on the screen
onScreenBullet :: Bullet -> Bool
onScreenBullet bullet = (posBullet bullet <= fromIntegral widthMap)

-- | Puts a tower in a certain coordinates and updates everything accordingly
putMarkAt :: ( (Int, Int)) -> Game -> Game
putMarkAt ( (i, j)) game 
  | insideMap (i,j) = game {
  board = if (money game >= actualCost) && checkCurrentPosition
    then (updateAt i (updateAt j place) (board game)) 
    else board game,
    
  money = currentMoney,
  
  enemies = takeWhile onScreenEnemy (enemies game),
  
  towers = if  (money game >= actualCost) 
  && checkCurrentPosition then
    ((newTower)) : updatedTowers 
    else  updatedTowers
  }
  | otherwise = game
  where
       updatedTowers = updateTowers (towers game) (enemies game)
       currentMoney = if (money game) >= actualCost && checkCurrentPosition then 
          money game - actualCost
          else money game
       checkCurrentPosition = checkPlaceList == [] && checkZombieList == []
       
       checkPlaceList = ( (filter (==(i,j)) (zip (map posTower(towers game)) 
         (map lineTower(towers game))  ) ))
       
       checkZombieList = ( (filter (==(i,j)) (zip (map round (map posEnemy(enemies game))) 
         (map lineEnemy(enemies game))  )  ))
       tower =  Just ( buildTower (i,j) (currentTower game))
       newTower = buildTower (i,j) (currentTower game)
       place (Nothing, _) =  (tower,(i,j))
       place a       = a
       actualCost = getPrice tower
       getPrice Nothing = 0
       getPrice (Just tower) = cost tower 
       
       
-- return true if we are in map        
insideMap :: (Int, Int) -> Bool
insideMap (i,j) = (i >=0) && (i<=widthMap - 1) && (j >=0) && (j <= lengthMap - 1)

-- This function draws one tower 
drawTower :: Tower -> Picture
drawTower tower = picTower tower

-- This function draws a list of tower using function that draws one tower
drawAllTowers :: [Tower] -> Picture
drawAllTowers [] = blank
drawAllTowers (x:xs) = translated (fromIntegral (posTower x)) 
  (fromIntegral (lineTower x)) (drawTower x) <> drawAllTowers xs
  
-- | This function draws the towers which are currently in the game
drawTowers :: Game -> Picture
drawTowers game = drawAllTowers (towers game)

-- This function draws special panel where player can choose tower
drawButtons :: Game -> Picture
drawButtons game = translated 1 10 (fireMagPicture) <> 
  translated 3 10 (frozenMagPicture) <> 
  translated 5 10 (elfPicture) <> 
  translated 7 10 (haskellPic) 

--creating bullets. Every type of tower has unique bullets
createBullet :: Tower -> Tower
createBullet tower = case typeTower tower of
  FireMag -> tower{
  bullets = fireBall(  fromIntegral (posTower tower), 
    (fromIntegral (lineTower tower))) : (bullets tower)
  }
  FrozenMag -> tower{
  bullets = frozenBall(  fromIntegral (posTower tower), 
    (fromIntegral (lineTower tower))) : (bullets tower)
  }
  Elf -> tower{
  bullets = arrow(  fromIntegral (posTower tower), 
    (fromIntegral (lineTower tower))) : (bullets tower)
  }
  Haskell -> tower{
  bullets = func(  fromIntegral (posTower tower), 
    (fromIntegral (lineTower tower))) : (bullets tower)
  }

-- | Checks whether the clicks are on map
clickOnMap :: (Int,Int) -> Bool
clickOnMap (x,y) = (x > widthMap) || (y > lengthMap) 
  
  
-- | Defines which tower was clicked
handleClick :: (Int,Int) -> Game -> Game 
handleClick (x, y) game = case (x,y) of
  (1,10) -> game{
    currentTower = FireMag
    }
  (3,10) -> game{
    currentTower = FrozenMag
    }
  (5,10) -> game{
    currentTower = Elf
    }  
  (7,10) -> game{
    currentTower = Haskell
    }  
  otherwise -> game 


--moving one bullet
movingBullet :: Bullet -> Bullet
movingBullet bullet = bullet {
  posBullet = posBullet bullet + speedBul bullet
  }


-- moving all bullets of one building  
movingBullets :: Tower -> Tower
movingBullets tower = tower {
  bullets = map (movingBullet) (bullets tower)
  }


-- | Convert mouse position into board coordinates.
pointToCoords :: Point -> (Int, Int)
pointToCoords (x, y) = (round (x / 1.2) + 4, round (y / 1.2) + 6)

--check if enemy is alive
isLife :: Enemy -> Bool
isLife enemy = hpEnemy enemy > 0


-- | Checks whether the bullets of the tower hit the enemy
hitEnemy :: Tower -> Enemy -> Enemy
hitEnemy tower enemy = foldr (hitOneEnemy) enemy (takeWhile onScreenBullet (bullets tower))


-- | Function that is responsible for hitting enemies
hitOneEnemy :: Bullet -> Enemy -> Enemy
hitOneEnemy bullet enemy 
  | (round(posBullet bullet) == round(posEnemy enemy)) 
  && ((lineBullet bullet) == (lineEnemy enemy)) 
  = (enemy {hpEnemy = (hpEnemy enemy) - damageBullet bullet})
  | otherwise = enemy
  
  
-- Checks every tower, whether they are hitting enemies or not
hitting :: [Tower] -> Enemy -> Enemy
hitting listTower enemy = foldr(hitEnemy) enemy listTower

-- | The list of towers and enemies to determine which is hitting which
hittingList :: ([Tower] -> Enemy -> Enemy) -> [Enemy] -> [Tower] -> [Enemy]
hittingList _ [] _ = []
hittingList f (x:xs) listTower
  | (hpEnemy nX) < 1 = hittingList f xs listTower
  | otherwise        = nX : hittingList f xs listTower
    where
      nX = f listTower x
      
-- | Changes the position of the enemy according to its speed
movingEnemy :: Enemy -> Enemy
movingEnemy enemy = enemy{
  posEnemy = posEnemy enemy - speed enemy
  }

-- | Function that is responsible for shooting bullets
shoot :: Enemy -> [Bullet] -> (Enemy, [Bullet])
shoot enemy [] = (enemy, [])
shoot enemy (bullet:bullets)
  | (lineBullet bullet) == (lineEnemy enemy) 
    && abs((posBullet bullet) - (posEnemy enemy)) < 0.1 
  = shoot nEnemy bullets -- 0.1 is used because == did not work
  | otherwise = (x, bullet:y)
  where
    nEnemy = (enemy {hpEnemy = (hpEnemy enemy) - (damageBullet bullet)})
    (x,y) = shoot enemy bullets

-- | Checks whether the bullets are hitting an enemy and deletes 
-- a bullet when damage is performed
doIt :: Enemy -> Tower -> (Enemy, Tower)
doIt enemy tower = (nEnemy, nTower)
  where
    (nEnemy, nBullet) = shoot enemy (bullets tower)
    nTower = (tower {bullets = nBullet})

-- | Checks which enemies can be reached by tower now
tryTower :: [Enemy] -> Tower -> ([Enemy], Tower)
tryTower [] tower = ([], tower)
tryTower (x:xs) tower = (nEnemies : newEnemies, newTower)
  where
    (nEnemies, nTower) = doIt x tower
    (newEnemies, newTower) = tryTower xs nTower

-- | Deletes the killed enemies
delDead :: [Enemy] -> [Enemy]
delDead [] = []
delDead (x:xs) 
  | (hpEnemy x) < 1 = delDead xs
  | otherwise       = x : delDead xs

-- | Checks every tower and enemy for hitting an enemy with a bullet
roundBul :: [Enemy] -> [Tower] -> ([Enemy], [Tower])
roundBul enemies [] = (enemies, [])
roundBul enemies (x:xs) = (delDead newEnemies, nX : nTowers)
  where
    (nEnemies, nX) = tryTower enemies x    
    (newEnemies, nTowers) = roundBul (delDead nEnemies) xs


-- | Checks whether tower is alive or not
isLifeTower :: Tower -> Bool
isLifeTower tower = (hpTower tower) > 0

-- | For tower, checks whether the enemy can hit it or not
tryToHit :: [Enemy] -> Tower -> Tower
tryToHit enemies tower = foldr (hitTower) tower enemies

-- | Reduces tower hp when it's eaten by enemies
hitTower :: Enemy -> Tower -> Tower
hitTower enemy tower = if (lineEnemy enemy == lineTower tower) 
  && (round (posEnemy enemy) == (posTower tower)) then
  tower {hpTower = (hpTower tower) - (damageEnemy enemy)}
  else tower
    
-- | Deletes towers
deleteTowers :: [Enemy] -> [Tower] -> [Tower]
deleteTowers enemies [] = []
deleteTowers enemies (x:xs) = tryToHit enemies x : deleteTowers enemies xs


-- | Gives a towers which an enemy can damage
updT :: Enemy -> [Tower] -> (Enemy, [Tower])
updT enemy [] = (enemy, [])
updT enemy (tower:towers)
  | (lineEnemy enemy) == (lineTower tower) && checkingPos
  && (damageEnemy enemy) >= (hpTower tower) 
    = (pEnemy, towers)
  | (lineEnemy enemy) == (lineTower tower) && checkingPos
    = (pEnemy, pTower : towers)
  | otherwise = (x, tower : y)
  where
    pEnemy = (enemy { posEnemy = fromIntegral (posTower tower) + 0.2})
    pTower = (tower { hpTower = (hpTower tower) - (damageEnemy enemy)})
    (x, y) = updT enemy towers
    checkingPos = abs((posEnemy enemy) - fromIntegral (posTower tower)) < 0.2 
      && (posEnemy enemy) > fromIntegral(posTower tower)
    
    
-- | For each enemy checks whether it can damage the tower
deleteTow :: [Enemy] -> [Tower] -> ([Enemy], [Tower])
deleteTow [] towers = ([], towers)
deleteTow (x:xs) towers = (nX : nXS, newTowers)
  where
    (nX, nTowers) = updT x towers
    (nXS, newTowers) = deleteTow xs nTowers
    



----------------------------------------------END GAME--------------------------
-- | check if at least one zombie came to the home
-- game over if this condition is true
correctState :: Game -> Bool
correctState game
 | length enemiesOnScreen == length (enemies game) = True
 | otherwise = False
  where
    enemiesOnScreen = filter onScreenEnemy (enemies game)


-- | Draw the whole game on a screen; if you lose -> Draw Game Over
drawAll :: Game -> Picture
drawAll game  
  | (gameOver game) = dilated 1.2 (translated (-4) (-6) (drawTowers game 
    <> drawEnemies game <> drawAllBullets game 
    <> drawMoney game <> drawTime game <> drawBoard game
      <> drawButtons game <> background game))
  | otherwise = colored red (scaled 4 4 endGame) <> background game

  -- | Handles the clicks in the game
handleGame :: Event -> Game -> Game
handleGame (PointerPress mouse) 
  | clickOnMap(pointToCoords mouse) = handleClick(pointToCoords mouse)
  | otherwise = putMarkAt ( pointToCoords mouse)
handleGame (TimePassing dt) = updateGame (dt )
handleGame _ = id  



run :: IO()
run = debugActivityOf (initBoard(widthMap, lengthMap)) 
  handleGame drawAll