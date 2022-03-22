module Solution.PermutationFlowShop.Functions.Acceptors where

import Solution.PermutationFlowShop.Types
import Solution.PermutationFlowShop.Functions.Evaluators (improvement)

naiveAcceptor :: Acceptor
naiveAcceptor _ _ _ = True

improvingAcceptor :: Acceptor
improvingAcceptor old new i = (improvement old new i) > 0
