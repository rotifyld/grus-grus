module Interpreter
    ( someFunc
    ) where

import qualified Data.Map as M
import ErrM

import AbsGrusGrus
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromMaybe)
import LexGrusGrus
import ParGrusGrus

data Value
    = VInt Integer
    | VBool Bool
    | VUnit
    | VClosure String Body Env
    deriving (Show)

class RuntimeExtract a where
    extract :: Value -> a

instance RuntimeExtract Integer where
    extract (VInt i) = i
    extract _ = error "TMP runtime extract int"

instance RuntimeExtract Bool where
    extract (VBool b) = b
    extract _ = error "TMP runtime extract bool"

--
--instance Num Value where
--    (+) (VInt i1) (VInt i2) = VInt (i1 + i2)
--    (+) _ _ = error "TMP adding non-numeric values"
--    (*) (VInt i1) (VInt i2) = VInt (i1 * i2)
--    (*) _ _ = error "TMP multiplying non-numeric values"
--    abs (VInt i) = VInt $ abs i
--    abs _ = error "TMP abs of non-numeric values"
--    signum (VInt i) = VInt $ signum i
--    signum _ = error "TMP signum of non-numeric values"
--    negate (VInt i) = VInt $ negate i
--    negate _ = error "TMP signum of non-numeric values"
--    fromInteger = VInt
type Loc = Int

type Env = M.Map String Loc

emptyEnv :: Env
emptyEnv = M.empty

lookupEnv :: String -> IM (Maybe Loc)
lookupEnv var = asks (M.lookup var)

fromEnv :: String -> IM Loc
fromEnv var = do
    mLoc <- lookupEnv var
    case mLoc of
        Just loc -> return loc
        Nothing -> error "TMP variable not in env"

insertEnv :: String -> Loc -> Env -> Env
insertEnv = M.insert

data Store =
    Store
        { storeMap :: M.Map Loc Value
        , storeNext :: Loc
        }

emptyStore :: Store
emptyStore = Store {storeMap = M.empty, storeNext = 0}

lookupStore :: Loc -> IM (Maybe Value)
lookupStore loc = gets (M.lookup loc . storeMap)

fromStore :: Loc -> IM Value
fromStore loc = do
    mVal <- lookupStore loc
    case mVal of
        Just val -> return val
        Nothing -> error "TMP location not in store"

insertStore :: Loc -> Value -> IM ()
insertStore loc val = modify (\s -> s {storeMap = M.insert loc val (storeMap s)})

allocStore :: IM Loc
allocStore = do
    newLoc <- gets storeNext
    modify (\s -> s {storeNext = newLoc + 1})
    return newLoc

type IM = ReaderT Env (State Store)

-- todo
--runIM :: IM a -> a
class Interpretable a where
    interpret :: a -> IM Value

instance Interpretable Body where
    interpret (Body [] e) = interpret e
    interpret (Body (DVal (Ident var) _ dExp:ds) bExp) = do
        v <- interpret dExp
        mLoc <- lookupEnv var
        loc <-
            case mLoc of
                Just l -> return l
                Nothing -> allocStore
        insertStore loc v
        local (M.insert var loc) $ interpret (Body ds bExp)
--    interpret (Body (DFun1 (Ident funName) (Ident varName) _ _ body:ds) bExp) = do
        

interpretBinaryOp :: (RuntimeExtract a) => Exp -> Exp -> (a -> a -> b) -> (b -> Value) -> IM Value
interpretBinaryOp e1 e2 op value = do
    v1 <- interpret e1
    v2 <- interpret e2
    return $ value $ extract v1 `op` extract v2

instance Interpretable Exp where
    interpret (EIfte eb e1 e2) = do
        vb <- interpret eb
        if extract vb
            then interpret e1
            else interpret e2
    interpret (EOr e1 e2) = interpretBinaryOp e1 e2 (||) VBool
    interpret (EAnd e1 e2) = interpretBinaryOp e1 e2 (&&) VBool
    interpret (EEq e1 e2) = interpretBinaryOp e1 e2 ((==) :: Integer -> Integer -> Bool) VBool
    interpret (ENeq e1 e2) = interpretBinaryOp e1 e2 ((/=) :: Integer -> Integer -> Bool) VBool
    interpret (ELt e1 e2) = interpretBinaryOp e1 e2 ((<) :: Integer -> Integer -> Bool) VBool
    interpret (EGt e1 e2) = interpretBinaryOp e1 e2 ((>) :: Integer -> Integer -> Bool) VBool
    interpret (ELe e1 e2) = interpretBinaryOp e1 e2 ((<=) :: Integer -> Integer -> Bool) VBool
    interpret (EGe e1 e2) = interpretBinaryOp e1 e2 ((>=) :: Integer -> Integer -> Bool) VBool
    interpret (EAdd e1 e2) = interpretBinaryOp e1 e2 (+) VInt
    interpret (ESub e1 e2) = interpretBinaryOp e1 e2 (-) VInt
    interpret (EMult e1 e2) = interpretBinaryOp e1 e2 (*) VInt
    interpret (EDiv e1 e2) = interpretBinaryOp e1 e2 div VInt
    interpret (EMod e1 e2) = interpretBinaryOp e1 e2 mod VInt
    interpret (ENot e) = do
        v <- interpret e
        return $ VBool $ not $ extract v
    interpret (EInt i) = return $ VInt i
    interpret (EBool BTrue) = return $ VBool True
    interpret (EBool BFalse) = return $ VBool False
    interpret (EUnit _) = return VUnit
    interpret (EVar (Ident var)) = fromStore =<< fromEnv var

-- todo wyrzucić do osobnego modułu
someFunc :: IO ()
someFunc = do
    putStrLn ""
    interact evalProgram
    putStrLn "\n"

evalProgram s =
    case pBody (myLexer s) of
        (Ok p) -> show $ evalState (runReaderT (interpret p) emptyEnv) emptyStore
        (Bad str) -> show str