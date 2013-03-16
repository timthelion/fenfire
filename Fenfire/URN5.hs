{-# LANGUAGE ImplicitParams #-}
module Fenfire.URN5 where

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

import Data.IORef
import System.Random (randomRIO)

type URIMaker = (String, IORef Integer)

newURIMaker :: IO URIMaker
newURIMaker = do rand <- sequence [randomRIO (0,63) | _ <- [1..27::Int]]
                 let chars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "+-"
                 ref <- newIORef 1
                 return ("urn:urn-5:" ++ map (chars !!) rand, ref)

newURI :: (?uriMaker :: URIMaker) => IO String
newURI = do let (base, ref) = ?uriMaker
            i <- readIORef ref; writeIORef ref (i+1)
            return (base ++ ":_" ++ show i)

