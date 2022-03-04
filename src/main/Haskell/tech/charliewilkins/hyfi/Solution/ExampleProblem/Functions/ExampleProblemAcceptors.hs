module Solution.ExampleProblem.Functions.ExampleProblemAcceptors where

import Solution.ExampleProblem.Types.ExampleProblemTypes
import Solution.ExampleProblem.Functions.ExampleProblemEvaluators

naiveAcceptor :: Acceptor
naiveAcceptor _ _ = True

improvingAcceptor :: Acceptor
improvingAcceptor old new = if (improvement old new) > 0 then True else False
