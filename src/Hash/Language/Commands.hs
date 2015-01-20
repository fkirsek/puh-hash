-- A map of (command name, command pairs), used to abstract command
-- execution and make adding new commands relatively easy

module Hash.Language.Commands where

import Hash.Language.Expressions
import Hash.Language.Exec 

commands :: Data.Map String Command

