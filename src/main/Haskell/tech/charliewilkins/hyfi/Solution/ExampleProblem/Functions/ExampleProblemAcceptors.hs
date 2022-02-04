module ExampleProblemAcceptors where

import ExampleProblemTypes
import ExampleProblemEvaluators

naiveAcceptor :: Acceptor
naiveAcceptor _ _ = True

improvingAcceptor :: Acceptor
improvingAcceptor old new = if (improvement old new) > 0 then True else False
