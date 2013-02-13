module RDF where

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

import Cache
import Utils

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set

data Node = URI String | PlainLiteral String    deriving (Eq, Ord)
data Dir  = Pos | Neg                           deriving (Eq, Ord, Show)

instance Show Node where
    show (URI uri)        = showURI [("rdfs", rdfs)] uri
    show (PlainLiteral s) = "\"" ++ s ++ "\""

type Triple = (Node, Node, Node)
type Side   = Map Node (Map Node (Set Node))
data Graph  = Graph Side Side (Set Triple) deriving (Show, Eq)

instance Hashable Node where
    hash (URI s) = hash s
    hash (PlainLiteral s) = hash s
    
instance Hashable Dir where
    hash Pos = 0
    hash Neg = 1

rdfs         =     "http://www.w3.org/2000/01/rdf-schema#"
rdfs_label   = URI "http://www.w3.org/2000/01/rdf-schema#label"
rdfs_seeAlso = URI "http://www.w3.org/2000/01/rdf-schema#seeAlso"

showURI ((short, long):xs) uri | take (length long) uri == long =
                                     short ++ ":" ++ drop (length long) uri
                               | otherwise = showURI xs uri
showURI [] uri = "<" ++ uri ++ ">"

subject :: Triple -> Node
subject (s,_,_) = s

predicate :: Triple -> Node
predicate (_,p,_) = p

object :: Triple -> Node
object (_,_,o) = o

graphSide :: Dir -> Graph -> Side
graphSide Neg (Graph s _ _) = s
graphSide Pos (Graph _ s _) = s

hasConn :: Graph -> Node -> Node -> Dir -> Bool
hasConn g node prop dir = isJust $ do m <- Map.lookup node (graphSide dir g)
                                      Map.lookup prop m

getOne :: Graph -> Node -> Node -> Dir -> Maybe Node
getOne g node prop dir = if null nodes then Nothing else Just $ head nodes
    where nodes = Set.toList (getAll g node prop dir)
    
getAll :: Graph -> Node -> Node -> Dir -> Set Node
getAll g node prop dir = 
    Map.findWithDefault Set.empty prop $ getConns g node dir

getConns :: Graph -> Node -> Dir -> Map Node (Set Node)
getConns g node dir = Map.findWithDefault Map.empty node $ graphSide dir g

emptyGraph :: Graph
emptyGraph = Graph (Map.empty) (Map.empty) Set.empty

listToGraph :: [Triple] -> Graph
listToGraph = foldr insert emptyGraph

graphToList :: Graph -> [Triple]
graphToList (Graph _ _ triples) = Set.toAscList triples

mergeGraphs :: Op Graph
mergeGraphs g1 g2 = foldr insertVirtual g1 (graphToList g2)

insert :: Triple -> Graph -> Graph
insert t (Graph neg pos triples) =
    insertVirtual t (Graph neg pos $ Set.insert t triples)

insertVirtual :: Triple -> Graph -> Graph
insertVirtual (s,p,o) (Graph neg pos triples) =
    Graph (ins o p s neg) (ins s p o pos) triples where
    ins a b c = Map.alter (Just . Map.alter (Just . Set.insert c . fromMaybe Set.empty) b . fromMaybe Map.empty) a   -- Gack!!! Need to make more readable
    
delete :: Triple -> Graph -> Graph
delete (s,p,o) (Graph neg pos triples) = 
    Graph (del o p s neg) (del s p o pos) $ Set.delete (s,p,o) triples where
    del a b c = Map.adjust (Map.adjust (Set.delete c) b) a
    
deleteAll :: Node -> Node -> Graph -> Graph
deleteAll s p g = dels s p os g where
    dels s' p' (o':os') g' = dels s' p' os' (delete (s',p',o') g')
    dels _  _  []       g' = g'
    os = Set.toList $ getAll g s p Pos
    
update :: Triple -> Graph -> Graph
update (s,p,o) g = insert (s,p,o) $ deleteAll s p g
    
triple :: Dir -> (Node,Node,Node) -> Triple
triple Pos (s,p,o) = (s,p,o)
triple Neg (o,p,s) = (s,p,o)

fromNode :: Node -> String
fromNode (URI uri)        = uri
fromNode (PlainLiteral s) = s

rev :: Dir -> Dir
rev Pos = Neg
rev Neg = Pos

mul :: Num a => Dir -> a -> a
mul Pos = id
mul Neg = negate
