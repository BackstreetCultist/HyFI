module ProblemModules.Example.MainTest where

import ProblemModules.Example.Main
import ProblemModules.Example.Types
import ProblemModules.Example.Functions.Operators
import ProblemModules.Example.Functions.Acceptors
import ProblemModules.Example.Functions.Evaluators

-- STARTUP --------------------------------------------------------------------
checkStartup :: IO ()
checkStartup = do
    return ()

-- OPERATORS ------------------------------------------------------------------
checkOperators = do
    return ()

-- ACCEPTORS ------------------------------------------------------------------
checkAcceptors = do
    return ()

-- EVALUATORS -----------------------------------------------------------------
checkEvaluators = do
    return ()

-- GENERAL --------------------------------------------------------------------
checkExample = do
    checkStartup
    checkOperators
    checkAcceptors
    checkEvaluators
