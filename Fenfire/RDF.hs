{-# LANGUAGE TypeOperators, ImplicitParams,ScopedTypeVariables,MultiParamTypeClasses,FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances, DeriveDataTypeable, OverlappingInstances,PatternGuards #-}

module Fenfire.RDF where

-- Copyright (c) 2006-2007, Benja Fallenstein, Tuukka Hastrup
-- This file is part of Fenfire.
-- 
-- Fenfire is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
-- 
-- Fenfire is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General
-- Public License for more details.
-- 
-- You should have received a copy of the GNU General
-- Public License along with Fenfire; if not, write to the Free
-- Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
-- MA  02111-1307  USA

import Fenfire.Cache
import Fenfire.Utils
import qualified Fenfire.Raptor as Raptor

import Control.Monad (liftM2)
import Control.Monad.Writer (Writer, WriterT, MonadWriter, tell, forM_,
                             runWriter, runWriterT)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (State, get, put, modify, runState)
import Control.Monad.Error () -- for the instance (Monad (Either String))

import Data.Generics hiding (empty)
import Data.List (intersperse)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, fromJust, isJust, listToMaybe)
import qualified Numeric
import Data.Set (Set)
import qualified Data.Set as Set

import Data.HList

import Network.URI hiding (query)

data Node = IRI { iriStr :: String }
          | BNode { bnodeGraph :: String, bnodeId :: String } 
          | Literal { literalStr :: String, literalTag :: LiteralTag }
     deriving (Eq, Ord, Show, Read, Typeable, Data)
data LiteralTag = Plain | Lang String | Type String 
     deriving (Eq, Ord, Show, Read, Typeable, Data)
data Dir  = Pos | Neg  deriving (Eq, Ord, Show)

{-instance Show Node where
    show = showNode defaultNamespaces-}
    

-- This is unfortunately something of a pun because I can't find a good
-- name for it: A 'coin' is something that has two sides...
class CoinClass c a | c -> a where
    getSide :: Dir -> c -> a
    
    getNeg :: c -> a; getNeg = getSide Neg
    getPos :: c -> a; getPos = getSide Pos
    
type Coin a = (a,a)

instance CoinClass (Coin a) a where
    getSide Neg = fst
    getSide Pos = snd


type Triple     = (Node, Node, Node)
type Namespaces = Map String String
    
data Conn = Conn { connProp :: Node, connDir :: Dir, connTarget :: Node }
            deriving (Eq, Ord, Show)
data Path = Path Node [Conn] deriving (Eq, Ord, Show)

pathToTriples :: Path -> [Triple]
pathToTriples (Path _ [])                 = []
pathToTriples (Path n (Conn p d n' : cs)) = 
    triple d (n,p,n') : pathToTriples (Path n' cs)

instance CoinClass Triple Node where
    getSide Neg = subject
    getSide Pos = object
    
instance CoinClass Path Node where
    getSide Neg (Path node _)     = node
    getSide Pos (Path node [])    = node
    getSide Pos (Path _    conns) = connTarget (last conns)

class Reversible r where
    rev :: Endo r
    
instance Reversible Dir where
    rev Neg = Pos; rev Pos = Neg
    
instance Reversible Path where
    rev (Path node conns) = foldr f (Path node []) (reverse conns) where
        f (Conn p d n') (Path n cs) = Path n' (Conn p (rev d) n : cs)
    
instance Hashable Node where
    hash (IRI s) = hash s
    hash (BNode g s) = hash (g,s)
    hash (Literal s Plain) = hash s
    hash (Literal s (Lang l)) = hash (s,l)
    hash (Literal s (Type t)) = hash (s,t)
    
instance Hashable Dir where
    hash Pos = 0
    hash Neg = 1

rdf          =     "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
rdf_type     = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"
rdf_List     = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"
rdf_first    = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"
rdf_next     = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#next"
rdf_nil      = IRI "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"

rdfs         =     "http://www.w3.org/2000/01/rdf-schema#"
rdfs_label   = IRI "http://www.w3.org/2000/01/rdf-schema#label"
rdfs_seeAlso = IRI "http://www.w3.org/2000/01/rdf-schema#seeAlso"

defaultNamespaces = Map.fromList [("rdf", rdf), ("rdfs", rdfs)]

showNode :: Namespaces -> Node -> String
showNode ns (IRI uri) = f (Map.toAscList ns) where
    f ((short, long):xs) | take (length long) uri == long =
                               short ++ ":" ++ drop (length long) uri
                         | otherwise = f xs
    f [] = "<" ++ turtle_escaped '>' uri ++ ">"
showNode _  (BNode graph id') = "bnode[" ++ id' ++ " @ " ++ graph ++ "]"
showNode ns (Literal lit tag) = "\"" ++ turtle_escaped '"' lit ++ "\"" ++
    case tag of Plain -> ""; Lang lang  -> "@" ++ lang;
                             Type type' -> "^^" ++ showNode ns (IRI type')

turtle_escaped :: Char -> String -> String
turtle_escaped _        [] = []
turtle_escaped c ('\\':xs) = '\\':'\\':turtle_escaped c xs
turtle_escaped c    (x:xs) | c == x 
                           = '\\':   c:turtle_escaped c xs
turtle_escaped c ('\n':xs) = '\\': 'n':turtle_escaped c xs
turtle_escaped c ('\r':xs) = '\\': 'r':turtle_escaped c xs
turtle_escaped c ('\t':xs) = '\\': 't':turtle_escaped c xs
turtle_escaped c    (x:xs) | i <- fromEnum x, i < 0x20 || i == 0x5C 
                           = '\\':'u':(p 4 (Numeric.showHex i "") $
                                       Numeric.showHex i (turtle_escaped c xs))
    where p n s rest | n > length s = '0':p n ('0':s) rest
                     | otherwise    = rest
turtle_escaped c (   x:xs) =         x:turtle_escaped c xs

subject :: Triple -> Node
subject (s,_,_) = s

predicate :: Triple -> Node
predicate (_,p,_) = p

object :: Triple -> Node
object (_,_,o) = o

fromGraph :: Pattern (Any,Any,Any,Any) r => Graph -> r
fromGraph = query (Any,Any,Any,Any)

fromDefaultGraph :: Pattern (Any,Any,Any,Dft) r => Graph -> r
fromDefaultGraph = query (Any,Any,Any,Dft)

class ToGraph a where toGraph :: Node -> a -> Graph -- node = default graph

instance ToGraph [Triple] where toGraph d = foldr insert (emptyGraph d)
instance ToGraph [Quad]   where toGraph d = foldr insertQuad (emptyGraph d)
instance ToGraph [x] => ToGraph (Set x) where 
    toGraph d = toGraph d . Set.toList
instance ToGraph [x] => ToGraph x where toGraph d x = toGraph d [x]

mergeGraphs :: Op Graph -- note: default graph and namespaces come from left
mergeGraphs g h = foldr insertQuad g (fromGraph h)

relativizeURI :: String -> Endo String
relativizeURI baseURI s = fromMaybe s $ do
    u <- Network.URI.parseURIReference s; bu <- Network.URI.parseURI baseURI
    return $ show $ Network.URI.relativeFrom u bu
    
absolutizeURI :: String -> Endo String
absolutizeURI baseURI s = fromMaybe s $ do
    u <- Network.URI.parseURIReference s; bu <- Network.URI.parseURI baseURI
    return $ show $ Network.URI.relativeTo u bu
    
relativizeNode :: String -> Endo Node
relativizeNode baseURI (IRI s) = IRI $ relativizeURI baseURI s
relativizeNode baseURI (BNode gid s) = BNode (relativizeURI baseURI gid) s
relativizeNode _ node = node

absolutizeNode :: String -> Endo Node
absolutizeNode baseURI (IRI s) = IRI $ absolutizeURI baseURI s
absolutizeNode baseURI (BNode gid s) = BNode (absolutizeURI baseURI gid) s
absolutizeNode _ node = node

changeBaseURI :: String -> String -> Endo Node
changeBaseURI oldBase newBase = absolutizeNode newBase . relativizeNode oldBase

setGraphURI :: String -> Endo Graph
setGraphURI uri g = 
    everywhereInGraph (changeBaseURI (iriStr $ defaultGraph g) uri) g

insert :: Triple -> Endo Graph
insert (s,p,o) g = insertQuad (s,p,o,defaultGraph g) g

delete :: Triple -> Endo Graph
delete (s,p,o) = delete' (s,p,o,Dft)

delete' :: Pattern pat [Quad] => pat -> Endo Graph
delete' pat g = foldr deleteQuad g (query pat g)
    
update :: Triple -> Endo Graph
update (s,p,o) = insert (s,p,o) . delete' (s,p,Any,Dft)

everywhereInGraph :: Endo Node -> Endo Graph
everywhereInGraph f g = setNamespaces (graphNamespaces g) $
                        toGraph (f $ defaultGraph g) $ everywhere (mkT f) $
                        (fromGraph g :: [Quad])

replaceNode :: Node -> Node -> Endo Graph
replaceNode m n = everywhereInGraph $ \x -> if x == m then n else x

setNamespaces :: Namespaces -> Endo Graph
setNamespaces ns g = g { graphNamespaces = ns }

addNamespace :: String -> String -> Endo Graph
addNamespace prefix uri g =
    g { graphNamespaces = Map.insert prefix uri $ graphNamespaces g }
    
triple :: Dir -> (Node,Node,Node) -> Triple
triple Pos (s,p,o) = (s,p,o)
triple Neg (o,p,s) = (s,p,o)

mul :: Num a => Dir -> a -> a
mul Pos = id
mul Neg = negate



--------------------------------------------------------------------------
-- FromRDF and ToRDF
--------------------------------------------------------------------------

{-
updateRDF :: (FromRDF a, ToRDF a) => Endo a -> Node -> Endo Graph
updateRDF f node graph = graph' where
    (x, ts) = runFromRDF $ readRDF graph node
    (_, ts') = runToRDF (iriStr $ defaultGraph graph) $ toRDF (f x)
    graph' = flip (foldr insert) (Set.toAscList ts') $
             foldr delete graph (Set.toAscList ts)
-}

type FromRdfM a = Graph -> Node -> Either String a

class FromRDF a where
    fromRDF :: FromRdfM a
    
type ToRdfM = State Graph

runToRDF :: Node -> ToRdfM Node -> (Node, Graph)
runToRDF graphNode m = runState m (emptyGraph graphNode)
    
nextNode :: (String -> [Node]) -> ToRdfM Node
nextNode f = do g <- get; return $ f' g (f $ iriStr $ defaultGraph g) where
    f' g (n:ns) = if not $ or [query (n,Any,Any) g, query (Any,n,Any) g,
                               query (Any,Any,n) g] then n else f' g ns
    f' _ [] = error "Fenfire.RDF.nextNode: finite list of choices"

newBNode :: ToRdfM Node
newBNode = nextNode $ \uri -> [BNode uri ("b" ++ show i) | i::Integer <- [1..]]

newFrag :: ToRdfM Node
newFrag = nextNode $ \uri -> [IRI (uri ++ "#" ++ show i) | i::Integer <- [1..]]

tellTs :: [Triple] -> ToRdfM ()
tellTs ts = modify (\g -> foldr insert g ts)

class ToRDF a where
    toRDF :: a -> ToRdfM Node
    
instance FromRDF a => FromRDF [a] where
    fromRDF g l = fromRDFList fromRDF g l
    
fromRDFList :: FromRdfM a -> FromRdfM [a]
fromRDFList f g l | l == rdf_nil = return []
                  | otherwise    = do
        first <- mquery (l, rdf_first, X) g
        rest <- mquery (l, rdf_next, X) g
        x  <- f g first
        xs <- fromRDFList f g rest
        return (x:xs)
        
instance ToRDF a => ToRDF [a] where
    toRDF = toRDFList toRDF
    
toRDFList _ []     = return rdf_nil
toRDFList f (x:xs) = do l <- newBNode; first <- f x; next <- toRDFList f xs
                        tellTs [ (l, rdf_first, first)
                               , (l, rdf_next, next) ]
                        return l

fromRDFPair :: Node -> FromRdfM a -> Node -> FromRdfM b -> FromRdfM (a,b)
fromRDFPair p1 f1 p2 f2 g n = do
    n1 <- query (n, p1, X) g; n2 <- query (n, p2, X) g
    v1 <- f1 g n1; v2 <- f2 g n2; return (v1,v2)

addRDFPair :: Node -> (a -> ToRdfM Node) -> Node -> (b -> ToRdfM Node)
           -> (a,b) -> Node -> ToRdfM ()
addRDFPair p1 f1 p2 f2 (x,y) n = do n1 <- f1 x; n2 <- f2 y
                                    tellTs [(n,p1,n1), (n,p2,n2)]

toRDFPair :: Node -> (a -> ToRdfM Node) -> Node -> (b -> ToRdfM Node)
          -> ((a,b) -> ToRdfM Node)
toRDFPair p1 f1 p2 f2 (x,y) =
    do n <- newBNode; addRDFPair p1 f1 p2 f2 (x,y) n; return n
    
fromRDFConn :: Node -> FromRdfM a -> FromRdfM a
fromRDFConn p f g n = query (n, p, X) g >>= f g

addRDFConn :: Node -> (a -> ToRdfM Node) -> a -> Node -> ToRdfM ()
addRDFConn p f x n = do n' <- f x; tellTs [(n,p,n')]
                                 
fromRDFConns :: Node -> FromRdfM a -> FromRdfM [a]
fromRDFConns p f g n = mapM (f g) $ query (n, p, X) g

addRDFConns :: Node -> (a -> ToRdfM Node) -> [a] -> Node -> ToRdfM ()
addRDFConns p f xs n = do nodes <- mapM f xs; tellTs [(n,p,n') | n' <- nodes]

toRDFConns :: Node -> (a -> ToRdfM Node) -> ([a] -> ToRdfM Node)
toRDFConns p f xs = do n <- newBNode; addRDFConns p f xs n; return n
            
instance FromRDF String where
    fromRDF _ (Literal s _) = return s
    fromRDF _ n = error $ "Fenfire.RDF.fromRDF(String): can only convert literals, not " ++ show n
    
instance ToRDF String where
    toRDF s = return (Literal s Plain)
    
instance FromRDF Node where
    fromRDF _ n = return n
    
instance ToRDF Node where
    toRDF n = return n


--------------------------------------------------------------------------
-- Raptor interface
--------------------------------------------------------------------------

raptorToGraph :: [Raptor.Triple] -> [(String, String)] -> String -> Graph
raptorToGraph raptorTriples namespaces graphURI' =
       setNamespaces (Map.fromList namespaces) (toGraph g triples) where
    g = IRI graphURI'
    triples = map convert raptorTriples
    convert (s,p,o) = (f s, f p, f o)
    f (Raptor.Uri s) = IRI s
    f (Raptor.Literal s) = Literal s Plain
    f (Raptor.Blank s) = BNode graphURI' s
    
graphToRaptor :: Graph -> ([Raptor.Triple], [(String, String)])
graphToRaptor graph = (map convert triples, namespaces) where
    uri       = iriStr $ defaultGraph graph
    graphURI' = flip fromMaybe (Network.URI.parseURI uri) $ error $
                "Fenfire.RDF.graphToRaptor: not a uri: \""++uri++"\""
    convert (s,p,o) = (f s, f p, f o)
    f (IRI s) = Raptor.Uri $ fromMaybe s $ do
                    u <- Network.URI.parseURI s
                    return $ show $ Network.URI.relativeFrom u graphURI'
    f (Literal s _) = Raptor.Literal s
    f (BNode g s) = if g == (iriStr $ defaultGraph graph) then Raptor.Blank s
                    else error "XXX Cannot save bnode from different graph"
    triples = query (Any,Any,Any,Dft) graph :: [Triple]
    namespaces = Map.toAscList $ graphNamespaces graph


--------------------------------------------------------------------------
-- Writing Turtle
--------------------------------------------------------------------------

{-
writeTurtle :: MonadWriter String m => String -> Graph -> m ()
writeTurtle nl graph = do let graph' = listToGraph $ graphToList graph
                              nss = graphNamespaces graph
                          writeTurtleNamespaces nl nss
                          writeTurtleStmts nl nss $ getPos graph'
                       
writeTurtleNamespaces nl nss = forM_ (Map.toAscList nss) $ \(prefix,iri) -> do
    tell "@prefix "; tell prefix; tell ": <"; tell iri; tell ">."; tell nl
    
writeTurtleStmts nl nss stmts = forM_ (Map.toAscList stmts) $ \(s,pos) -> do
    tell nl; writeTurtleNode nss s; tell nl
    sequence_ $ intersperse (tell ";" >> tell nl) $
        map (writeTurtlePred nl nss) $ Map.toAscList pos
    tell "."; tell nl
    
writeTurtlePred nl nss (p, os) = do
    tell "  "; writeTurtleNode nss p; tell nl
    sequence_ $ intersperse (tell "," >> tell nl) $
        map (writeTurtleObj nss) $ Set.toAscList os
    
writeTurtleObj nss o = do tell "    "; writeTurtleNode nss o

writeTurtleNode nss node = tell $ showNode nss node
-}

writeTriples nss ts = concatMap (writeTriple nss) ts

writeTriple nss (s,p,o) =
    (concat $ intersperse " " $ map (showNode nss) [s,p,o]) ++ ".\n"


--------------------------------------------------------------------------
-- Reimplementation, using HList; this will become the default
-- once it's finished
--------------------------------------------------------------------------

data Any = Any   deriving (Eq, Ord, Show)
data X = X       deriving (Eq, Ord, Show)
data Dft = Dft deriving (Eq, Ord, Show) -- pattern matching the default graph

-- Examples:
-- query (x, rdf_type, X)   :: Graph -> Set Node
-- query (x, rdf_type, X)   :: Graph -> Maybe Node
-- query (x, rdf_type, Any) :: Graph -> Set Triple
-- query (x, rdf_type, Any) :: Graph -> Maybe Triple
-- There are lots of other combinations.
class Show pattern => Pattern pattern result where
    query :: pattern -> Graph -> result
    
type Quad = (Node, Node, Node, Node)

quad2triple :: Quad -> Triple
quad2triple (s,p,o,_) = (s,p,o)

quadSubject :: Quad -> Node
quadSubject (s,_,_,_) = s

quadPredicate :: Quad -> Node
quadPredicate (_,p,_,_) = p

quadObject :: Quad -> Node
quadObject (_,_,o,_) = o

quadGraph :: Quad -> Node
quadGraph (_,_,_,g) = g

data Graph = Graph { defaultGraph :: Node
                   , graphNamespaces :: Namespaces
                   , graphViews :: Map (Node, Node, Node, Node) (Set Quad)
                               :*: Map (Node, Node, Node, Any)  (Set Quad)
                               :*: Map (Node, Node, Any,  Node) (Set Quad)
                               :*: Map (Node, Node, Any,  Any)  (Set Quad)
                               :*: Map (Node, Any,  Node, Node) (Set Quad)
                               :*: Map (Node, Any,  Node, Any)  (Set Quad)
                               :*: Map (Node, Any,  Any,  Node) (Set Quad)
                               :*: Map (Node, Any,  Any,  Any)  (Set Quad)
                               :*: Map (Any,  Node, Node, Node) (Set Quad)
                               :*: Map (Any,  Node, Node, Any)  (Set Quad)
                               :*: Map (Any,  Node, Any,  Node) (Set Quad)
                               :*: Map (Any,  Node, Any,  Any)  (Set Quad)
                               :*: Map (Any,  Any,  Node, Node) (Set Quad)
                               :*: Map (Any,  Any,  Node, Any)  (Set Quad)
                               :*: Map (Any,  Any,  Any,  Node) (Set Quad)
                               :*: Map (Any,  Any,  Any,  Any)  (Set Quad)
                               :*: HNil -- use some simple TH for this? :-)
                   } deriving (Eq, Typeable)
                   
instance Show Graph where
    show g@(Graph d ns _) = "setNamespaces " ++ show ns ++ " (toGraph " ++
                            show d++" "++show (fromGraph g :: [Quad])++")"
                   
instance (Empty x, Empty xs) => Empty (HCons x xs) where 
    empty = HCons empty empty
instance Empty HNil where empty = HNil

emptyGraph d = Graph d defaultNamespaces empty

simpleQuery pattern g = 
    Map.findWithDefault Set.empty pattern $ hOccursFst (graphViews g)

-- We need an instance for each of these because the code GHC *generates*
-- for each of these is different, even though *we* write the same thing
-- for each. Again, we should use Template Haskell to generate these.
instance Pattern (Node, Node, Node, Node) (Set Quad) where query = simpleQuery
instance Pattern (Node, Node, Node, Any)  (Set Quad) where query = simpleQuery
instance Pattern (Node, Node, Any,  Node) (Set Quad) where query = simpleQuery
instance Pattern (Node, Node, Any,  Any)  (Set Quad) where query = simpleQuery
instance Pattern (Node, Any,  Node, Node) (Set Quad) where query = simpleQuery
instance Pattern (Node, Any,  Node, Any)  (Set Quad) where query = simpleQuery
instance Pattern (Node, Any,  Any,  Node) (Set Quad) where query = simpleQuery
instance Pattern (Node, Any,  Any,  Any)  (Set Quad) where query = simpleQuery
instance Pattern (Any,  Node, Node, Node) (Set Quad) where query = simpleQuery
instance Pattern (Any,  Node, Node, Any)  (Set Quad) where query = simpleQuery
instance Pattern (Any,  Node, Any,  Node) (Set Quad) where query = simpleQuery
instance Pattern (Any,  Node, Any,  Any)  (Set Quad) where query = simpleQuery
instance Pattern (Any,  Any,  Node, Node) (Set Quad) where query = simpleQuery
instance Pattern (Any,  Any,  Node, Any)  (Set Quad) where query = simpleQuery
instance Pattern (Any,  Any,  Any,  Node) (Set Quad) where query = simpleQuery
instance Pattern (Any,  Any,  Any,  Any)  (Set Quad) where query = simpleQuery

instance (Show p, Show o, Show g, Pattern (Any,p,o,g) (Set Quad)) => 
         Pattern (X,p,o,g) (Set Node) where
    query (X,p,o,g) = Set.map (subject . quad2triple) . query (Any,p,o,g)
instance (Show s, Show o, Show g, Pattern (s,Any,o,g) (Set Quad)) => 
         Pattern (s,X,o,g) (Set Node) where
    query (s,X,o,g) = Set.map (predicate . quad2triple) . query (s,Any,o,g)
instance (Show s, Show p, Show g, Pattern (s,p,Any,g) (Set Quad)) => 
         Pattern (s,p,X,g) (Set Node) where
    query (s,p,X,g) = Set.map (object . quad2triple) . query (s,p,Any,g)
instance (Show s, Show p, Show o, Pattern (s,p,o,Any) (Set Quad)) => 
         Pattern (s,p,o,X) (Set Node) where
    query (s,p,o,X) = Set.map quadGraph . query (s,p,o,Any)
    
instance (Show s, Show p, Show o, Pattern (s,p,o,Node) (Set Quad)) =>
         Pattern (s,p,o,Dft) (Set Quad) where
    query (s,p,o,Dft) g = query (s, p, o, defaultGraph g) g
    
instance (Show s, Show p, Show o, Pattern (s,p,o,Any) (Set Quad)) => 
         Pattern (s,p,o) (Set Quad) where
    query (s,p,o) = query (s,p,o,Any)

instance (Show s, Show p, Show o, Pattern (s,p,o,Any) (Set Node)) => 
         Pattern (s,p,o) (Set Node) where
    query (s,p,o) = query (s,p,o,Any)


instance Pattern pat (Set Quad) => Pattern pat (Set Triple) where
    query pat = Set.map quad2triple . query pat

instance Pattern pat (Set r) => Pattern pat [r] where
    query pat = Set.toList . query pat

instance Pattern pat (Set r) => Pattern pat (Either String r) where
    query pat g = let s = query pat g in 
                  if Set.null s then Left $ "Pattern not found: " ++ show pat
                                else Right $ Set.findMin s
                                
instance Pattern pat (Either String r) => Pattern pat (Maybe r) where
    query = mquery
    
instance Pattern pat (Set Quad) => Pattern pat Bool where
    query pat = not . Set.null . (id :: Endo (Set Quad)) . query pat

mquery :: (Pattern pat (Either String r), Monad m) => pat -> Graph -> m r
mquery pat g = either fail return $ query pat g

query' :: Pattern pat (Either String r) => pat -> Graph -> r
query' pat g = either error id $ query pat g

iquery :: (?graph :: Graph, Pattern pat r) => pat -> r
iquery pat = query pat ?graph

imquery :: (?graph :: Graph, Pattern pat (Either String r), Monad m) => 
           pat -> m r
imquery pat = mquery pat ?graph

iquery' :: (?graph :: Graph, Pattern pat (Either String r)) => pat -> r
iquery' pat = query' pat ?graph
    
    
class (Ord a, Show a) => PatternSlot a where toPatternSlot :: Node -> a
instance PatternSlot Node where toPatternSlot = id
instance PatternSlot Any where toPatternSlot _ = Any

newtype InsertQuad = InsertQuad Quad; newtype DeleteQuad = DeleteQuad Quad

instance (PatternSlot s, PatternSlot p, PatternSlot o, PatternSlot g) => 
         Apply InsertQuad (Map (s,p,o,g) (Set Quad)) 
                          (Map (s,p,o,g) (Set Quad)) where
    apply (InsertQuad (s,p,o,g)) =
      updateWithDefault Set.empty (Set.insert (s,p,o,g))
        (toPatternSlot s, toPatternSlot p, toPatternSlot o, toPatternSlot g)

instance (PatternSlot s, PatternSlot p, PatternSlot o, PatternSlot g) => 
         Apply DeleteQuad (Map (s,p,o,g) (Set Quad)) 
                          (Map (s,p,o,g) (Set Quad)) where
    apply (DeleteQuad (s,p,o,g)) =
      updateWithDefault Set.empty (Set.delete (s,p,o,g))
        (toPatternSlot s, toPatternSlot p, toPatternSlot o, toPatternSlot g)

insertQuad :: Quad -> Endo Graph
insertQuad q g = g { graphViews = hMap (InsertQuad q) $ graphViews g }

deleteQuad :: Quad -> Endo Graph
deleteQuad q g = g { graphViews = hMap (DeleteQuad q) $ graphViews g }
