module StandardLibrary
    ( initialTypecheckEnv
    , initialExecuteEnv
    ) where

import AbsGrusGrus

import qualified ExecutorUtils as E
import qualified TypecheckerUtils as T
import Utils

data StdLibFunction =
    StdLibFunction Name [(Name, T.Type)] T.Type (Body Pos)
stdLib :: [StdLibFunction]
stdLib =
    [ (StdLibFunction "not" [("b", T.TBool)] T.TBool
           (Body noPos []
                (EIfte noPos (EVar noPos (LIdent "b")) (EBool noPos (BFalse noPos)) (EBool noPos (BTrue noPos)))))
    ]
    
toValue :: E.Env -> StdLibFunction -> E.Value
toValue env (StdLibFunction funName typedParams _ body) =
    E.VFun $ E.Function (Just funName) (map fst typedParams) body env

toType :: StdLibFunction -> T.Type
toType (StdLibFunction _ typedParams funType _) = T.TArrow (map snd typedParams) funType

toName :: StdLibFunction -> Name
toName (StdLibFunction name _ _ _) = name

initialTypecheckEnv :: T.Env
initialTypecheckEnv = T.addVariablesEnv (map toName stdLib) (map toType stdLib) T.emptyEnv

initialExecuteEnv :: E.Env
initialExecuteEnv = env
  where
    env = E.addManyEnv (map toName stdLib) (map (toValue E.emptyEnv) stdLib) E.emptyEnv