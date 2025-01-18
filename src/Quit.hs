module Quit(checkQuitApplication) where

import ProgramState (ProgramState)
import Control.Monad (guard)

quitSign :: String
quitSign = "Quiting application"

checkQuitApplication :: ProgramState -> Maybe ProgramState
checkQuitApplication state@(_, str, _) = guard (str/=quitSign) >> Just state -- Ako stringa ne e raven na znaka za izhod 
                                                                             -- vurni steyta za izhod ako li ne vrushta nishto
                                                                             -- Sori che e na shlyokavica
