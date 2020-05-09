module StandardLibrary
    ( initialTypecheckEnv
    , initialExecuteEnv
    ) where

import AbsGrusGrus
import ExecutorUtils (Function(..), Value(..))
import qualified ExecutorUtils as Executor (Env, addManyEnv, emptyEnv)
import TypecheckerUtils (Type(..))
import qualified TypecheckerUtils as Typechecker (Env, addVariablesEnv, emptyEnv)
import Utils

data StdLibFunction =
    StdLibFunction Name [(Name, Type)] Type (Body Pos)

stdLib :: [StdLibFunction]
stdLib = [(StdLibFunction "not" [("b", TBool)] TBool (Body noPos [] (EIfte noPos (EVar noPos (LIdent "b")) (EBool noPos (BFalse noPos)) (EBool noPos (BTrue noPos)))))]

toValue :: Executor.Env -> StdLibFunction -> Value
toValue env (StdLibFunction funName typedParams _ body) = VFun $ Function (Just funName) (map fst typedParams) body env

toType :: StdLibFunction -> Type
toType (StdLibFunction _ typedParams funType _) = TArrow (map snd typedParams) funType

toName :: StdLibFunction -> Name
toName (StdLibFunction name _ _ _) = name

initialTypecheckEnv :: Typechecker.Env
initialTypecheckEnv = Typechecker.addVariablesEnv (map toName stdLib) (map toType stdLib) Typechecker.emptyEnv

initialExecuteEnv :: Executor.Env
initialExecuteEnv = env
  where
    env = Executor.addManyEnv (map toName stdLib) (map (toValue Executor.emptyEnv) stdLib) Executor.emptyEnv