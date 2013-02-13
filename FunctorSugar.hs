{-# OPTIONS_GHC -fth #-} 
module FunctorSugar where

import Control.Applicative
import Control.Monad
import Control.Monad.Writer
import Data.Maybe
import Language.Haskell.TH
import System.IO.Unsafe

functorCall :: Functor f => f a -> a
functorCall x = error "Eeene meene miste es rappelt in der kiste"

fzip :: Applicative f => f a -> f b -> f (a,b)
fzip a b = pure (\x y -> (x,y)) <*> a <*> b

fcurry :: Applicative f => (f (a,b) -> c) -> f a -> f b -> c
fcurry f x y = f (fzip x y)

functorSugar :: ExpQ -> ExpQ
functorSugar expQ = do exp <- expQ
                       (exp', calls) <- runWriterT (traverse exp)
                       appsE ([mkFMap $ length calls,
                               lamE (map (varP . fst) calls) $ return exp']
                            ++ map (return . snd) calls) 

mkFMap :: Int -> ExpQ
mkFMap 0 = [| pure |]
mkFMap 1 = [| fmap |]
mkFMap n = [| \f -> $(repeatFn (n-1) [| fcurry |]) 
                        (fmap ($(repeatFn (n-1) [| uncurry |]) f)) |]
    where repeatFn :: Int -> ExpQ -> ExpQ
          repeatFn 1 e = e
          repeatFn n e = [| $e . $(repeatFn (n-1) e) |]

callExpr :: Exp
callExpr = unsafePerformIO $ runQ [| FunctorSugar.functorCall |]

type Binds = [(Name, Exp)]
type Traverse a = a -> WriterT Binds Q a

traverse :: Traverse Exp
traverse e = case e of
    VarE name -> return (VarE name)
    ConE name -> return (ConE name)
    LitE lit -> return (LitE lit)
    AppE e1 e2 | e1 == callExpr -> do name <- lift $ newName "call"
                                      tell [(name, e2)]
                                      return (VarE name)
    AppE e1 e2 -> liftM2 AppE (traverse e1) (traverse e2)
    InfixE el ei er -> do ei' <- traverse ei
                          el' <- maybe (return Nothing) 
                                       (liftM Just . traverse) el
                          er' <- maybe (return Nothing) 
                                       (liftM Just . traverse) er
                          return (InfixE el' ei' er')
    LamE pats e -> liftM (LamE pats) (traverse e)
    TupE exps -> liftM TupE (mapM traverse exps)
    LetE decls e -> liftM2 LetE (mapM traverseDec decls) (traverse e)
    ListE exps -> liftM ListE (mapM traverse exps)
    SigE e type' -> liftM (flip SigE type') (traverse e)
    DoE stmts -> liftM DoE (mapM traverseStmt stmts)
    CompE stmts -> liftM CompE (mapM traverseStmt stmts)
    e -> error ("expression type not implemented yet: " ++ show e)
    
traverseDec :: Traverse Dec
traverseDec decl = case decl of
    FunD name clauses -> liftM (FunD name) (mapM traverseClause clauses)
    ValD pat body decls -> liftM2 (ValD pat) (traverseBody body)
                                             (mapM traverseDec decls)
    SigD name type' -> return (SigD name type')
    -- ... possibly other things but I <benja> think not ... --
    d -> error ("declaration type not implemented: " ++ show d)

traverseClause :: Traverse Clause
traverseClause (Clause pats body decls) =
    liftM2 (Clause pats) (traverseBody body) (mapM traverseDec decls)
    
traverseBody :: Traverse Body
traverseBody (NormalB e) = liftM NormalB (traverse e)
traverseBody (GuardedB bs) = liftM GuardedB $ forM bs $ \(g,e) ->
    do e' <- traverse e; return (g,e')
    
traverseStmt :: Traverse Stmt
traverseStmt stmt = case stmt of
    BindS pat exp -> liftM (BindS pat) (traverse exp)
    LetS decs -> liftM LetS (mapM traverseDec decs)
    NoBindS exp -> liftM NoBindS (traverse exp)
    ParS stmts -> liftM ParS (mapM (mapM traverseStmt) stmts)

{-
CondE Exp Exp Exp
LetE [Dec] Exp
CaseE Exp [Match]
ArithSeqE Range
ListE [Exp]
SigE Exp Type
RecConE Name [FieldExp]
RecUpdE Exp [FieldExp]
-}
