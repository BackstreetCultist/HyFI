module Solution.BinPacking.Functions.BinPackingAcceptors where

import Solution.BinPacking.Functions.BinPackingEvaluators (improvement)
import Solution.BinPacking.Types.BinPackingTypes

naiveAcceptor :: Acceptor
naiveAcceptor _ _ _ = True

improvingAcceptor :: Acceptor
improvingAcceptor old new i = (improvement old new i) > 0
