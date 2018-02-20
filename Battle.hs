
import Control.Monad.State.Lazy

import ExceptStuff
import StateStuff
import MonadStuff
import MonadTransF



data BChr = NoBChr | BChr { getID :: Int , getName :: String , getMaxHP :: Int , getHP :: Int , getPowerLevel :: Int , getSpeed :: Int , getDefTable :: DefTable , getTurnHandler :: TurnHandler , getMoveTable :: MoveTable, getStatuses :: Statuses }

type BData = [(Int,BChr)]

type TurnHandler = ExceptT String (StateT BData IO) ()

type DefTable = [(DefType,DefValue)]

data DefType = MathD | FinanceD | ScienceD | ComputerD | SadnessD
                    | EthicsD | PhysicalD | AdminD | MetaD
  deriving Eq

type DefValue = Double

data 

