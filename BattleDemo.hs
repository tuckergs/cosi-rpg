
import System.Random

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy

import Monads.StateStuff
import Monads.ExceptStuff
import Monads.Constructs

import Battle


makeReece :: BChr
makeReece = BChr { getChrID = "reece" , getChrName = "Reece" , getMaxHP = 100 , 
                    getHP = 100 , getPowerLevel = 10 ,
                    getInspiration = 50 , getMaxInspiration = 50 ,
                    getTeam = LTeam , getDefTable = reeceDefTable ,
                    getTurnHandler = humanTurnHandler ,
                    getMoveTable = reeceMoveTable ,
                    getDeathHandler = reeceDeathHandler ,
                    getStatusTable = emptyStatusTable ,
                    getPrevCmd = "wat" ,
                    getTmpDmg = 0 , getTmpEle = MetaE 
                 }

emptyStatusTable = [(PassiveS,[]),(GettingAttackedS,[]),
                    (WhileAttackingS,[]),(GettingHealedS,[]),
                    (WhileHealingS,[]),(HardCodeS,[])]

reeceDefTable = [(MathE,1.2),(FinanceE,0.9),(ScienceE,1.1),(ComputerE,0.8),
                (SadnessE,0.9),(EthicsE,1.0),(PhysicalE,1.0),(MemesE,0.7),
                (AdminE,1.1),(MetaE,1.0)]

reeceDeathHandler _ = fromIO $ do
  putStrLn ""
  putStrLn "Oh noes! You\'ve succumbed to the Clarkson!"
  putStrLn "Better luck next time!"

reeceMoveTable = [("rockfarm",reeceRockFarm),("plus6",reecePlus6)]

reeceRockFarm = Move { getMoveID = "rockfarm" , getMoveName = "Rock Farm" ,
                        getTargetMode = EnemyTM , inspirationNeeded = 0 ,
                        getMoveDescription = "Make rocks, so you can make more money than the opponent you select",
                        getMoveCode = reeceRockFarmCode
                     }


reeceRockFarmCode :: [ChrID] -> BattleIO ()
reeceRockFarmCode (rID:eID:_) = do
  rName <- fmap getChrName $ getChr rID
  eName <- fmap getChrName $ getChr eID
  atk <- fmap getPowerLevel $ getChr rID
  let pawaa = fromIntegral atk
  fromIO $ putStrLn $ rName ++ " makes rocks and thus more money than " ++ eName ++ "!"
  damageTarget rID eID FinanceE $ pawaa
  
reecePlus6  = Move { getMoveID = "plus6" , getMoveName = "+6!" ,
                      getTargetMode = SelfTM , inspirationNeeded = 5 ,
                      getMoveDescription = "Harness the magical abilities of staying up til 4am!",
                      getMoveCode = reecePlus6Code
                    }


reecePlus6Code :: [ChrID] -> BattleIO ()
reecePlus6Code (rID:_) = do
  rndNum0 <- (fromRNG $ randomR (0,3)) :: BattleIO Int
  when (rndNum0 == 0) $ do
    fromIO $ putStrLn $ "Reece got some sleep!"
    healTarget rID rID MemesE $ 6
  rndNum <- (fromRNG $ randomR (0,5)) :: BattleIO Int
  -- 0, get 10 inspiration
  -- 1-2, get healing status
  -- 3-4, get attack status
  -- 5, you don't got the bagels
  when (rndNum == 0) $ receiveInspiration rID 10
  when (rndNum `elem` [1,2]) $ applyStatus rID dipset
  when (rndNum `elem` [3,4]) $ applyStatus rID dBagel
  when (rndNum == 5) $ forM_ youdontgotnobagel $ applyStatus rID


    
  
dipset = Status { getStatusType = GettingHealedS , getStatusName = "Dipset!" ,
                  getStatusDescription = "Increases heal efficiency by 20 percent, for free!" ,
                  getPriority = 0 , getNumTurns = TurnNum 5 ,
                  getStatusCode = modTmpDmg (1.2*) ,
                  getWornCode = const $ return () ,
                  isHidden = False
                }

dBagel = Status { getStatusType = WhileAttackingS , getStatusName = "Deterministic bagels" ,
                  getStatusDescription = "Sleep depravation causes you to deal 20 percent more damage!",
                  getPriority = 0 , getNumTurns = TurnNum 7 ,
                  getStatusCode = modTmpDmg (1.2*) ,
                  getWornCode = const $ return () ,
                  isHidden = False
                }

youdontgotnobagel :: [Status]
youdontgotnobagel = ls
  where ls = [
              theSt { isHidden = False } ,
              theSt { getStatusType = PassiveS } ,
              theSt { getStatusType = GettingHealedS } ,
              theSt { getStatusType = GettingAttackedS } 
             ]
        theSt = Status { getStatusType = HardCodeS , getStatusName = "You don\'t got the bagels!" ,
                          getStatusDescription = "Damage heals you, healing hurts you, by the same magnitude" ,
                          getPriority = -841 , getNumTurns = TurnNum 5 ,
                          getStatusCode = modTmpDmg negate ,
                          getWornCode = const $ return () ,
                          isHidden = True
                       }
  


makeTino = BChr { getChrID = "tino" , getChrName = "Tino" , getMaxHP = 500 , 
                    getHP = 500 , getPowerLevel = 5 ,
                    getInspiration = 100000 , getMaxInspiration = 100000 ,
                    getTeam = RTeam , getDefTable = tinoDefTable ,
                    getTurnHandler = tinoTurnHandler ,
                    getMoveTable = [] ,
                    getDeathHandler = tinoDeathHandler ,
                    getStatusTable = emptyStatusTable ,
                    getPrevCmd = "" ,
                    getTmpDmg = 0 , getTmpEle = MetaE 
                 }

tinoDeathHandler _ = do
  fromIO $ putStrLn $ ""
  fromIO $ putStrLn $ "Tino relents!"
  fromIO $ putStrLn $ "You graduated two years early!"
  fromIO $ putStrLn $ "Go get some cake, and probably a job as well!"

tinoDefTable = [(MathE,0.8),(FinanceE,1.1),(ScienceE,0.8),(ComputerE,0.8),
                (SadnessE,0.9),(EthicsE,0.5),(PhysicalE,1.0),
                (AdminE,1.1),(MetaE,1.0)]

tinoTurnHandler tID = do
  rndNum <- (fromRNG $ randomR (0,49) :: BattleIO Int)
  let rID = "reece"
  when (rndNum `elem` [0..39]) $ fmap (const ()) $ runMove tID tinoEnigma [tID,rID]
  when (rndNum `elem` [40,41]) $ fmap (const ()) $ runMove tID tinoTest [tID,rID]
  when (rndNum `elem` [42..49]) $ fmap (const ()) $ runMove tID tinoCurve [tID,rID]
  -- when (rndNum `elem` [40..49]) $ fmap (const ()) $ runMove tID tinoTest [tID,rID]
  
tinoEnigma = Move { getMoveID = "enigma" , getMoveName = "Tino Enigma" ,
                      getTargetMode = EnemyTM , inspirationNeeded = 0 ,
                      getMoveDescription = "Do damage of a random element (Math,CS,Science,Meta)",
                      getMoveCode = tinoEnigmaCode
                    }

tinoEnigmaCode [tID,eID] = do
  rndNum <- (fromRNG $ randomR (0,3) :: BattleIO Int)
  ele <- startSwitch MetaE $ do
    swCase (rndNum == 0) $ return MathE
    swCase (rndNum == 1) $ return ComputerE
    swCase (rndNum == 2) $ return ScienceE
    swCase (rndNum == 3) $ return MetaE
  atk <- fmap getPowerLevel $ getChr tID
  let pawaa = fromIntegral atk
  fromIO $ putStrLn "Tino blows your mind!"
  when (rndNum `elem` [0,1,2]) $
    fromIO $ putStrLn $ "With " ++ (show ele) ++ "(tm)!"
  when (rndNum == 3) $ do
    rndAsgnNum <- (fromRNG $ randomR (1,4) :: BattleIO Int)
    fromIO $ putStrLn $ "By asking tuckergs to decrement your health manually for Assignment " ++ (show rndAsgnNum) ++ "!"
  damageTarget tID eID ele pawaa


tinoCurve = Move { getMoveID = "curve" , getMoveName = "Tino Curve" ,
                      getTargetMode = EnemyTM , inspirationNeeded = 0 ,
                      getMoveDescription = "Tino curves a previous test, so you gain health" ,
                      getMoveCode = tinoCurveCode
                    }

tinoCurveCode [tID,eID] = do
  fromIO $ putStrLn $ "Tino is a nice and merciful professor"
  rndNum <- (fromRNG $ randomR (0,1)) :: BattleIO Int
  when (rndNum == 0) $ do
    fromIO $ putStrLn $ "Have some health!"
    healTarget tID eID ComputerE 30
  when (rndNum == 1) $ do
    fromIO $ putStrLn $ "Have some inspiration!"
    receiveInspiration eID 10

tinoTest = Move { getMoveID = "test" , getMoveName = "Tino Test" ,
                      getTargetMode = EnemyTM , inspirationNeeded = 0 ,
                      getMoveDescription = "Tino assigns a test! Be ready!" ,
                      getMoveCode = tinoTestCode
                    }

tinoTestCode [tID,eID] = do
  fromIO $ putStrLn $ "Tino assigned a test! Be ready!"
  forM_ tinoTestStatuses $ applyStatus eID

tinoTestStatuses = [ infoSt , timerSt ]
  where 
    infoSt = Status { getStatusType = HardCodeS , getStatusName = "Tino test!" ,
                      getStatusDescription = "Tino will give a test when this status wears. Be ready!" ,
                      getPriority = 0 , getNumTurns = TurnNum 3 ,
                      getStatusCode = const $ return () ,
                      getWornCode = const $ return () ,
                      isHidden = False
                     }
    timerSt = Status { getStatusType = HardCodeS , getStatusName = "Tino test!" ,
                      getStatusDescription = "Tino will give a test when this status wears. Be ready!" ,
                      getPriority = 0 , getNumTurns = TurnNum 2 ,
                      getStatusCode = const $ return () ,
                      getWornCode = timerWornCode ,
                      isHidden = True
                     }
    timerWornCode = \chrID -> do
      fromIO $ putStrLn $ "Test next turn! Be ready!"
      applyStatus chrID realSt
    realSt = Status { getStatusType = PassiveS , getStatusName = "Tino test!" ,
                      getStatusDescription = "Tino will give a test when this status wears. Be ready!" ,
                      getPriority = -729 , getNumTurns = TurnNum 1 ,
                      getStatusCode = \chrID -> do { 
                        fromIO $ putStrLn $ "Test time!" ;
                        fromIO $ putStrLn $ "You see many true/false questions, and you say, that\'s easy" ;
                        fromIO $ putStrLn $ "But on second glance, you found out that they were hard" ;
                        fromIO $ putStrLn $ "Computer science can be painful sometimes" ;
                        passiveDamage chrID SadnessE 9999
                      },
                      getWornCode = const $ return () ,
                      isHidden = True
                     }
                     -- TODO: I need to think about my passive damage system
                     -- I was thinking that passive should be damaging and gives inspiration
                     -- Functions called passiveDamage, passiveHeal

handleBattleIO :: StdGen -> BEnv -> BattleIO () -> IO ()
handleBattleIO gn benv m = fmap (const ()) $ runStateTF gn $ runStateTF benv $ do
  res <- runExceptT m
  case res of
    Left _ -> return ()
    Right _ -> lift $ lift $ putStrLn "Happy birthday! The battle ended without returning a result"
  
  
  
  
  

main = do
  gn <- getStdGen
  let benv = [("reece",makeReece),("tino",makeTino)]
  putStrLn "You are about to get a degree, but you have to pass a Tino class first"
  handleBattleIO gn benv battleLoop
    
  

