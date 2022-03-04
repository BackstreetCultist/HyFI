module Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityAcceptors where

import Solution.BooleanSatisfiability.Types.BooleanSatisfiabilityTypes
import Solution.BooleanSatisfiability.Functions.BooleanSatisfiabilityEvaluators (improvement)

naiveAcceptor :: Acceptor
naiveAcceptor _ _ _ = True

improvingAcceptor :: Acceptor
improvingAcceptor old new i = (improvement old new i) > 0
