
module Battle where

import Control.Monad
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class

import System.Random

import Monads.ExceptStuff
import Monads.StateStuff
import Monads.MonadStuff
import Monads.MonadTransF
import Monads.SParseT

import Helpers

import Data.List (sort)
import Data.Bits (xor)


-- Structures

type BattleIO = ExceptT BResult (StateT BEnv (StateT StdGen IO))

data BResult = LWin | RWin | Tie | Exit

type ChrID = String

type MvID = String

data BTeam = LTeam | RTeam
  deriving Eq

data BChr = NoBChr | BChr { getChrID :: ChrID , getChrName :: String , 
              getMaxHP :: Int , getHP :: Int , getPowerLevel :: Int , 
              getInspiration :: Int , getMaxInspiration :: Int ,
              getTeam :: BTeam , getDefTable :: DefTable , 
              getMoveTable :: MoveTable , getStatusTable :: StatusTable ,
              getTurnHandler :: ChrID -> BattleIO () , 
              getDeathHandler :: ChrID -> BattleIO () ,
              getPrevCmd :: String ,
              getTmpDmg :: Double , getTmpEle :: Element }

instance Eq BChr where
  ch1 == ch2 = getChrID ch1 == getChrID ch2

type BEnv = [(ChrID,BChr)]

type DefTable = [(Element,DefValue)]

data Element = MathE | FinanceE | ScienceE | ComputerE | SadnessE
                    | EthicsE | PhysicalE | MemesE | AdminE | MetaE
  deriving Eq

instance Show Element where
  show MathE = "Math"
  show FinanceE = "Finance"
  show ScienceE = "Science"
  show ComputerE = "CS"
  show SadnessE = "Sadness"
  show EthicsE = "Irresponsible Adult"
  show PhysicalE = "Physical"
  show MemesE = "Memes"
  show AdminE = "Admin"
  show MetaE = "Meta"

type DefValue = Double

type MoveTable = [(MvID,Move)]

data TargetMode = AnyTM | EnemyTM | AllyTM | SelfTM
  deriving Eq

instance Show TargetMode where
  show AnyTM = "Any"
  show EnemyTM = "Enemies only"
  show AllyTM = "Allies only"
  show SelfTM = "Self only"

data Move = Move { getMoveID :: MvID , getMoveName :: String , 
                  getTargetMode :: TargetMode , inspirationNeeded :: Int ,
                  getMoveDescription :: String , 
                  getMoveCode :: [ChrID] -> BattleIO () 
                 }

type StatusTable = [(StatusType,[Status])]

data StatusType = PassiveS | WhileAttackingS | GettingAttackedS
                  | WhileHealingS | GettingHealedS | HardCodeS
  deriving Eq

data Status = Status { getStatusType :: StatusType , getStatusName :: String , 
                      getStatusDescription :: String , 
                      getPriority :: Int , getNumTurns :: TurnNum , 
                      getStatusCode :: ChrID -> BattleIO () , 
                      getWornCode :: ChrID -> BattleIO () ,
                      isHidden :: Bool }

instance Eq Status where
  st1 == st2 = getPriority st1 == getPriority st2

instance Ord Status where
  st1 <= st2 = getPriority st1 >= getPriority st2

data TurnNum = TurnNum Int | InfinityTN

instance Show TurnNum where
  show (TurnNum t) = show t
  show InfinityTN = "Infinity"

-- Lifters

fromIO :: IO a -> BattleIO a
fromIO = lift . lift . lift

fromRNG :: (StdGen -> (a,StdGen)) -> BattleIO a
fromRNG f = lift . lift $ StateT $ \g -> return $ f g


-- BEnv manipulation

getEnv :: BattleIO BEnv
getEnv = lift $ get

putEnv :: BEnv -> BattleIO ()
putEnv = lift . put

removeChrFromEnv :: ChrID -> BattleIO ()
removeChrFromEnv i = do
  env <- getEnv
  let env2 = [ p | p <- env , fst p /= i ]
  putEnv env2

replaceChr :: BChr -> BEnv -> BEnv
replaceChr _ [] = []
replaceChr newChr ((curID,curChr):rest)
  | newChr == curChr = (curID,newChr):rest
  | otherwise = (curID,curChr):(replaceChr newChr rest)

putChrToEnv :: BChr -> BattleIO ()
putChrToEnv chr = do
  env <- getEnv
  putEnv $ replaceChr chr env

-- Monty Python references in Haskell
-- This function removes the characters who have nonpositive health
-- It also checks to see if a team has been defeated, and exits as such
bringOutYourDead :: BattleIO ()
bringOutYourDead = do
  deadChrs <- lift $ onlyAction $ return . filter (\(_,chr) -> getHP chr <= 0)
  lift $ onlyState $ return . filter (\(_,chr) -> getHP chr > 0)
  mapM_ (\(chrID,chr) -> getDeathHandler chr chrID) deadChrs
  leftSurvivors <- lift $ onlyAction $ return . filter (\(_,chr) -> (getTeam chr == LTeam))
  rightSurvivors <- lift $ onlyAction $ return . filter (\(_,chr) -> (getTeam chr == RTeam))
  when (leftSurvivors == [] && rightSurvivors == []) $ brek Tie
  when (leftSurvivors == []) $ brek RWin
  when (rightSurvivors == []) $ brek LWin


  

getChrMaybe :: ChrID -> BattleIO (Maybe BChr)
getChrMaybe i = do
  env <- getEnv
  return $ lookup i env

getChr :: ChrID -> BattleIO BChr
getChr i = do
  cm <- getChrMaybe i
  case cm of
    Just chr -> return chr
    Nothing -> error $ "Could not retrieve character with id " ++ (show i)

getMoveMaybe :: ChrID -> MvID -> BattleIO (Maybe Move)
getMoveMaybe chrID mvID = do
  chr <- getChr chrID
  return $ lookup mvID $ getMoveTable chr


-- RNG

getRNG :: BattleIO StdGen
getRNG = lift . lift $ get




-- Battle loop stuff

decrementTurnNum :: TurnNum -> TurnNum
decrementTurnNum (TurnNum t) = TurnNum (t-1)
decrementTurnNum InfinityTN = InfinityTN

numTurnsIsPositive :: TurnNum -> Bool
numTurnsIsPositive InfinityTN = True
numTurnsIsPositive (TurnNum t) = (t > 0)

getNextChr :: BattleIO ChrID
getNextChr = lift $ onlyAction $ \((e,_):_) -> return e

nextTurn :: BattleIO ()
nextTurn = lift $ onlyState $ \(e:env) -> return $ env ++ [e]

handleStatuses :: ChrID -> BattleIO ()
handleStatuses cID = do
  table <- fmap getStatusTable $ getChr cID
  let passiveStatusCodes = map getStatusCode $ sort $ brutalLookup PassiveS table
  -- Passive statuses affect character
  mapM_ ($ cID) passiveStatusCodes
  let 
    splitTable = \worn -> do
      (ty,ls) <- table
      let
        ls2 = do
          st <- ls
          let newNumTurns = decrementTurnNum $ getNumTurns st
          guard ( worn `xor` (numTurnsIsPositive newNumTurns) )
          return $ st { getNumTurns = newNumTurns }
      return (ty,ls2)
    table2 = splitTable False
    wornStatusCodes = map getWornCode $ sort $ concat $ map snd $ splitTable True
  -- Remove worn statuses
  modChrBy cID $ \chr -> chr { getStatusTable = table2 }
  -- Worn handlers
  mapM_ ($ cID) wornStatusCodes

  
battleLoop :: BattleIO ()
battleLoop = forever $ do
  fromIO $ putStrLn ""
  curChrID <- getNextChr
  turnHandler <- fmap getTurnHandler $ getChr curChrID
  turnHandler curChrID
  bringOutYourDead
  curChrStillAlive <- getChrMaybe curChrID
  case curChrStillAlive of
    Just _ -> do
      handleStatuses curChrID
      bringOutYourDead
      nextTurn
    Nothing -> nextTurn

-- Human turn handling

runMove :: ChrID -> Move -> [ChrID] -> BattleIO Bool
runMove yourID mv ls = do
  yourName <- fmap getChrName $ getChr yourID
  yourInsp <- fmap getInspiration $ getChr yourID
  let requiredInsp = inspirationNeeded mv
  if (yourInsp < requiredInsp) 
    then do
      fromIO $ putStrLn $ "More inspiration is required to use this move!"
      return False
    else do
      when (requiredInsp > 0) $ fromIO $ do
        putStrLn $ yourName ++ " spends " ++ (show requiredInsp) ++ " inspiration!"
        putStrLn $ yourName ++ " now has " ++ (show $ yourInsp - requiredInsp) ++ " inspiration!"
      fromIO $ putStrLn $ yourName ++ " uses " ++ getMoveName mv ++ "!"
      modChrBy yourID $ \chr -> chr { getInspiration = yourInsp - requiredInsp }
      getMoveCode mv ls
      return True


humanTurnParser :: ChrID -> String -> BattleIO Bool
humanTurnParser yourID prompt = fmap fst $ runStateTF prompt $ fmap snd $ runStateTF False $ do
  cmd <- lift $ getNextWord
  let prevCmds = ["prev","pr"]
  let possibleCmds = prevCmds ++ ["wat","chrs","statusOf","moves","do","info","i","exit"]
  onlyState $ when' (cmd == "wat") $ lift $ fromIO $ do
    putStrLn "chrs: Tells you which characters live along with their ids"
    putStrLn "moves: Tells you the moves of the current character"
    putStrLn "(statusOf/info/i) [chrID]: Tells you the current status of a character"
    putStrLn "do [mvID (self move)]: Perform a self-targeting move"
    putStrLn "do [mvID (any/enemy/ally move)] [targetID]: Perform a character targeting move"
    putStrLn "(prev/pr): Re-run the previous \"do\" action"
    putStrLn "exit: Exit the battle"
    return False
  onlyState $ when' (cmd `elem` prevCmds) $ lift $ do
    prevFull <- fmap getPrevCmd $ getChr yourID
    fromIO $ putStrLn $ prevFull
    humanTurnParser yourID prevFull
  onlyState $ when' (cmd == "chrs") $ lift $ do
    env <- getEnv
    fromIO $ putStrLn "Here are the characters who are alive"
    fromIO $ forM_ env $ 
      \(chrID,chr) -> putStrLn $ getChrName chr ++ " ( with id " ++ chrID ++ " )"
    fromIO $ putStrLn "You can use these ids to look up info with statusOf"
    return False
  onlyState $ when' (cmd `elem` ["statusOf","info","i"]) $ do
    chrID <- getNextWord
    chrMaybe <- lift $ getChrMaybe chrID
    case chrMaybe of
      Nothing -> do
        lift $ fromIO $ putStrLn $ chrID ++ " is not a valid character id"
        return False
      Just chr -> lift $ fromIO $ do
        putStrLn $ "Status of " ++ (getChrName chr) ++ " (" ++ chrID ++ ")"
        putStrLn $ "HP: " ++ (show $ getHP chr) ++ "/" ++ (show $ getMaxHP chr)
        putStrLn $ "IP: " ++ (show $ getInspiration chr) ++ "/" ++ (show $ getMaxInspiration chr)
        let allStatuses = concat $ map snd $ getStatusTable chr
        putStrLn $ "Statuses:"
        stRes <- forM allStatuses $ 
          \st -> if (isHidden st) then return () else do
            putStrLn $ (getStatusName st)
            putStrLn $ "  Turns left: " ++ (show $ getNumTurns st)
            putStrLn $ "  Description: " ++ (getStatusDescription st)
        when (null stRes) $ putStrLn "(no statuses)"
        return False
  onlyState $ when' (cmd == "moves") $ do
    mvTable <- fmap getMoveTable $ lift $ getChr yourID
    lift $ fromIO $ putStrLn "Here are your moves:"
    lift $ fromIO $ forM_ mvTable $ \(mvID,mv) -> do
      putStrLn $ "Name: " ++ getMoveName mv ++ " (id " ++ mvID ++ ")"
      putStrLn $ "  Description: " ++ getMoveDescription mv
      putStrLn $ "  Targets: " ++ (show $ getTargetMode mv)
      putStrLn $ "  Inspiration needed: " ++ (show $ inspirationNeeded mv)
    return False
  onlyState $ when' (cmd == "do") $ do
    mvID <- getNextWord
    mvMaybe <- lift $ getMoveMaybe yourID mvID 
    case mvMaybe of
      Nothing -> do
        lift $ fromIO $ putStrLn $ mvID ++ " is not a valid move id"
        return False
      Just mv -> do
        let targetMode = getTargetMode mv 
        tarID <- getNextWord
        lift $ fmap snd $ runStateTF False $ do -- I'm done with parsing the word
          onlyState $ when' (targetMode == SelfTM) $ do
            if (tarID `elem` ["",yourID])
              then runMove yourID mv [yourID]
              else do
                fromIO $ putStrLn "Self targeting moves must be of the form:"
                fromIO $ putStrLn "  do mvID / do mvID yourID"
                return False
          onlyState $ when' (targetMode `elem` [AnyTM,AllyTM,EnemyTM]) $ do
            yourTeam <- ((return . getTeam) <=< getChr) $ yourID
            tarMaybe <- getChrMaybe tarID
            case tarMaybe of
              Nothing -> do
                fromIO $ putStrLn $ tarID ++ " is not a valid character id"
                return False
              Just tar -> do
                let tarTeam = getTeam tar
                let allyTMAndDiff = targetMode == AllyTM && tarTeam /= yourTeam
                let enemyTMAndSame = targetMode == EnemyTM && tarTeam == yourTeam
                when allyTMAndDiff $ fromIO $ putStrLn $ "You can\'t target an enemy with an ally-targeting attack"
                when enemyTMAndSame $ fromIO $ putStrLn $ "You can\'t target an ally with an enemy-targeting attack"
                if (not $ allyTMAndDiff || enemyTMAndSame) 
                  then runMove yourID mv [yourID,tarID]
                  else return False
  onlyState $ when' (cmd == "exit") $ lift $ do
    fromIO $ putStrLn $ "Are you sure? (type y for yes, anything else for no)"
    exitResponse <- fromIO $ getLine
    if (exitResponse == "y")
      then brek Exit
      else return False
  onlyState $ when' (not $ elem cmd possibleCmds) $ do
    lift $ fromIO $ putStrLn $ "Invalid command!"
    return False
  when (cmd == "do") $
    lift $ lift $ modChrBy yourID $ \chr -> chr { getPrevCmd = prompt }
                
humanTurnHandler :: ChrID -> BattleIO ()
humanTurnHandler chrI = do
  curChr <- getChr chrI
  let name = getChrName curChr
  fromIO $ putStrLn $ "It\'s " ++ name ++ "\'s turn to move!"
  doStuff
  where 
    doStuff = do
      fromIO $ putStrLn $ "What will you do? Type wat for help"
      response <- fromIO $ getLine
      fromIO $ putStrLn $ ""
      finished <- humanTurnParser chrI response
      when (not finished) doStuff

-- Functions that are expected to be used!

-- Received damage modifier

modTmpDmg :: (Double -> Double) -> ChrID -> BattleIO ()
modTmpDmg f = \chrID -> modChrBy chrID $ 
  \chr -> chr { getTmpDmg = f $ getTmpDmg chr }
--modTmpDmg f = \ch -> return $ ch { getTmpDmg = f $ getTmpDmg ch }

-- Modify character in some way

modChrBy :: ChrID -> (BChr -> BChr) -> BattleIO ()
modChrBy chrID f = do
  chr <- getChr chrID
  putChrToEnv $ f chr

-- Defense handling (DONUT USE!) (Used for damageTarget)

applyDefense :: Status
applyDefense = Status { getStatusType = PassiveS , getStatusName = "" , 
                        getStatusDescription = "", getPriority = 0,
                        getNumTurns = InfinityTN ,
                        getStatusCode = \chrID -> do {
                          ele <- (fmap getTmpEle) $ getChr chrID ; 
                          table <- (fmap getDefTable) $ getChr chrID ;
                          let multiplier = brutalLookup ele table
                          in modTmpDmg (*multiplier) chrID
                        },
                        getWornCode = const $ return () ,
                        isHidden = True
                      }
  
applyHealBonus :: Status
applyHealBonus = Status { getStatusType = PassiveS , getStatusName = "" , 
                        getStatusDescription = "", getPriority = 0,
                        getNumTurns = InfinityTN ,
                        getStatusCode = \chrID -> do {
                          ele <- (fmap getTmpEle) $ getChr chrID ; 
                          table <- (fmap getDefTable) $ getChr chrID ;
                          let multiplier = brutalLookup ele table
                          in modTmpDmg (/multiplier) chrID
                        },
                        getWornCode = const $ return () ,
                        isHidden = True
                      }


-- Inspiration affecting monads

-- This function is not to be used to use inspiration
receiveInspiration :: ChrID -> Int -> BattleIO ()
receiveInspiration chrID num = do
  name <- fmap getChrName $ getChr chrID
  oldInsp <- fmap getInspiration $ getChr chrID
  maxInsp <- fmap getMaxInspiration $ getChr chrID
  let newInsp = oldInsp + num
  fromIO $ putStrLn $ name ++ " received " ++ (show num) ++ " inspiration!"
  let realNewInsp = max 0 $ min maxInsp newInsp
  fromIO $ putStrLn $ name ++ " now has " ++ (show realNewInsp) ++ " inspiration!"
  modChrBy chrID $ \chr -> chr { getInspiration = max 0 $ min maxInsp newInsp }

-- HP affecting monads

modHP :: ChrID -> Double -> BattleIO ()
modHP chrID num = do
  oldHP <- fmap getHP $ getChr chrID
  maxHP <- fmap getMaxHP $ getChr chrID
  chrName <- fmap getChrName $ getChr chrID
  let newHP = oldHP + (iint num)
  let realNewHP = min maxHP newHP
  fromIO $ putStrLn $ chrName ++ " now has " ++ (show realNewHP) ++ " HP!"
  modChrBy chrID $ \chr -> chr { getHP = realNewHP }
 

damageTarget :: ChrID -> ChrID -> Element -> Double -> BattleIO ()
damageTarget atkI tarI ele dmg = do
  attacker <- getChr atkI
  target <- getChr tarI
  let attackerStatuses = brutalLookup WhileAttackingS $ getStatusTable attacker
  let targetStatuses = brutalLookup GettingAttackedS $ getStatusTable target
  let relevantStatuses = sort $ (applyDefense:) $ attackerStatuses ++ targetStatuses 
  -- Set up received damage, element
  modChrBy tarI (\chr -> chr { getTmpDmg = dmg , getTmpEle = ele })
  -- Run statuses
  mapM_ ((flip getStatusCode) tarI) relevantStatuses
  -- Get received damage
  newDmg <- fmap getTmpDmg $ getChr tarI
  -- Modify health
  fromIO $ putStrLn $ (getChrName attacker) ++ " dealt " ++ (show $ iint newDmg) 
    ++ " " ++ (show ele) ++ " damage to " ++ (getChrName target) ++ "!"
  modHP tarI (negate newDmg)

healTarget :: ChrID -> ChrID -> Element -> Double -> BattleIO ()
healTarget atkI tarI ele dmg = do
  attacker <- getChr atkI
  target <- getChr tarI
  let attackerStatuses = brutalLookup WhileHealingS $ getStatusTable attacker
  let targetStatuses = brutalLookup GettingHealedS $ getStatusTable target
  let relevantStatuses = sort $ (applyHealBonus:) $ attackerStatuses ++ targetStatuses 
  -- Set up received damage, element
  modChrBy tarI (\chr -> chr { getTmpDmg = dmg , getTmpEle = ele })
  -- Run statuses
  mapM_ ((flip getStatusCode) tarI) relevantStatuses
  -- Get received damage
  newDmg <- fmap getTmpDmg $ getChr tarI
  -- Modify health
  fromIO $ putStrLn $ (getChrName attacker) ++ " healed " ++ (getChrName target) ++  " by " ++ (show $ iint newDmg) ++ "! (" ++ (show ele) ++ ")"
  modHP tarI newDmg

passiveDamage :: ChrID -> Element -> Double -> BattleIO ()
passiveDamage tarI ele dmg = do
  target <- getChr tarI
  let targetStatuses = brutalLookup GettingAttackedS $ getStatusTable target
  let relevantStatuses = sort $ (applyDefense:) $ targetStatuses 
  -- Set up received damage, element
  modChrBy tarI (\chr -> chr { getTmpDmg = dmg , getTmpEle = ele })
  -- Run statuses
  mapM_ ((flip getStatusCode) tarI) relevantStatuses
  -- Get received damage
  newDmg <- fmap getTmpDmg $ getChr tarI
  -- Modify health
  fromIO $ putStrLn $ (getChrName target) ++ " took " ++ (show $ iint newDmg) 
    ++ " " ++ (show ele) ++ " damage over time!"
  modHP tarI (negate newDmg)

-- TODO: Copy and past to make passiveHeal

-- Status application

applyStatus :: ChrID -> Status -> BattleIO ()
applyStatus chrID st = do
  name <- fmap getChrName $ getChr chrID
  stTable <- fmap getStatusTable $ getChr chrID
  let stType = getStatusType st
      relevantStatuses = (st:) $ brutalLookup stType $ stTable
      newStTable = ((stType,relevantStatuses):) $
        filter (\(ty,_) -> stType /= ty) stTable
  when (not $ isHidden st) $
    fromIO $ putStrLn $ name ++ " caught the common " ++ (getStatusName st)
  modChrBy chrID $ \chr -> chr { getStatusTable = newStTable }



