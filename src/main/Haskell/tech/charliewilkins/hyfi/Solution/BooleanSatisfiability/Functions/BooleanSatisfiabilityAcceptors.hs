module BooleanSatisfiabilityAcceptors where

import BooleanSatisfiabilityTypes
import BooleanSatisfiabilityEvaluators (improvement)

naiveAcceptor :: Acceptor
naiveAcceptor _ _ _ = True

improvingAcceptor :: Acceptor
improvingAcceptor old new i = (improvement old new i) > 0
