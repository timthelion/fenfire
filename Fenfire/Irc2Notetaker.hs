{-# OPTIONS_GHC -fglasgow-exts -fffi #-}
module Fenfire.Irc2Notetaker where

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

import System.Time (getClockTime, toUTCTime, CalendarTime(..), ClockTime(..), 
                    toClockTime)
import System.Environment (getArgs, getProgName)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafeInterleaveIO)

import Network.URI (escapeURIString, isUnreserved)
import System.Cmd (rawSystem)

import Data.Char (ord, toUpper, toLower, isLetter, isSpace)
import Data.Bits ((.&.))
import Data.List (isPrefixOf, intersperse)
import qualified Data.Map as Map

import Control.Monad (when)
import qualified Control.Exception

import System.Glib.UTFString (newUTFString, readCString, 
                              peekUTFString)
import System.Glib.FFI (withCString, nullPtr, CString, CInt, Ptr)
import System.IO.Unsafe (unsafePerformIO)

foreign import ccall "g_utf8_validate" valid :: CString -> CInt -> 
                                                Ptr (CString) -> Bool

-- XXX real toUTF isn't exported from System.Glib.UTFString
toUTF :: String -> String
toUTF s = unsafePerformIO $ newUTFString s >>= readCString

fromUTF :: String -> String
fromUTF s = unsafePerformIO $ Control.Exception.catch 
    (withCString s $ \cstr -> peekUTFString cstr >>= \s' -> 
     if (valid cstr (-1) nullPtr && all isUnicode s') -- force any exceptions
         then return s'  -- it really was utf-8
         else return s ) -- it really wasn't utf-8
    (\_e -> return s)                 -- if any, keep the local encoding

-- from gutf8.c used in g_utf8_validate
isUnicode c' = let c = ord c' in
     c < 0x110000 &&
     c .&. 0xFFFFF800 /= 0xD800 &&
     (c < 0xFDD0 || c > 0xFDEF) &&
     c .&. 0xFFFE /= 0xFFFE


-- command line parsing and event dispatching
main = do (root,channels,parseTimeStamps) <- do
              args <- getArgs
              case args of 
                  [root,channels] ->
                      return (root,channels,False)
                  ["-t",root,channels] -> 
                      return (root,channels,True)
                  _ -> error "[-t] root channels"
          when (not $ "http://" `isPrefixOf` root)
               $ error "The root doesn't start with http://"
          (irclines,timestamps) <- case parseTimeStamps of
                  False -> do
                      irc <- getContents
                      timestamps <- getTimeStamps
                      return (lines irc,timestamps)
                  True -> do
                      irc <- getContents >>= return . lines
                      let (firsts, rests) = unzip $ map (span (/= ' ')) irc
                      return (map (drop 1) rests, map parseTime firsts)

          mapM_ (uncurry $ handle (words channels) root) 
                $ zip (map fromUTF irclines) (uniquify timestamps)

-- a lazy stream of timestamps based on evaluation time
getTimeStamps = do ~(TOD secs _picos) <- unsafeInterleaveIO getClockTime
                   xs <- unsafeInterleaveIO getTimeStamps
                   return (TOD secs 0:xs)

-- parser for timestamp input format yyyy-mm-ddThh:mm:ss+zhzm
parseTime :: String -> ClockTime
parseTime str =  toClockTime $ CalendarTime 
                     (read year) (toEnum $ read month-1) (read day)
                     (read hour) (read minute) (read second) 
                     0 (toEnum 0) 0 "" ((read tzh*60+read tzm)*60) False 
    where (year,   rest0) = span (/= '-') str
          (month,  rest1) = span (/= '-') $ drop 1 rest0
          (day,    rest2) = span (/= 'T') $ drop 1 rest1
          (hour,   rest3) = span (/= ':') $ drop 1 rest2
          (minute, rest4) = span (/= ':') $ drop 1 rest3
          (second, rest5) = span (/= '+') $ drop 1 rest4
          (tzh,    tzm  ) = splitAt 2     $ drop 1 rest5
                  
-- differentiating consecutive equivalent elements by adding a sequence number
uniquify     [] = []
uniquify (x:xs) = (x,Nothing):uniquify' (x,Nothing) xs

uniquify'    _     [] = []
uniquify' prev (x:xs) | fst prev == x = next prev:uniquify' (next prev) xs
                      | otherwise     =   first x:uniquify' (first x) xs
    where next (i,offset) = (i, Just $ maybe (2::Integer) (+1) offset)
          first i         = (i, Nothing)

-- event handling by posting queries
handle :: [String] -> String -> 
          String -> (ClockTime, Maybe Integer) -> IO ()
handle channels root line time = do 
    let query = irc2notetaker channels time (parse line)
    maybe (return ()) (\xs -> rawSystem "curl" ["--data",build xs,root] >> return ()) query

build = concat . intersperse "&" . map build'
    where build' (key,value) = key ++ "=" ++ escapeURIString isUnreserved 
                                                             (toUTF value)

-- parsing of a line in irc protocol syntax
parse (':':rest) = (Just $ takeWhile (/=' ') rest,
                    parse' "" (tail $ dropWhile (/=' ') rest))
parse      rest  = (Nothing, parse' "" rest)

parse' acc       [] = [reverse acc]
parse' acc   ['\r'] = [reverse acc]
parse'  "" (':':xs) = [reverse . dropWhile (=='\r') $ reverse xs]
parse' acc (' ':xs) = reverse acc : parse' "" xs
parse' acc   (x:xs) = parse' (x:acc) xs

-- maybe query for a given irc event
irc2notetaker :: [String] -> (ClockTime, Maybe Integer) -> 
                 (Maybe String,[String]) -> Maybe [(String, String)]
irc2notetaker channels (time,_offset) ((Just prefix),[cmd,target,msg0])
    | map toUpper cmd == "PRIVMSG", 
      '#':channelName <- map toLower target, channelName `elem` channels
    = 
    let msg = case msg0 of '+':cs -> cs; '-':cs -> cs; cs -> cs
    in
    Just [("nick",nick),("line",msg)]
    where 
          nick = takeWhile (/='!') prefix
          (CalendarTime y moe d h m s _ps _wd _yd _tzn _tz _isDST) 
              = toUTCTime time
          mo = (fromEnum moe+1)
          p n i = take (n-length (show i)) (repeat '0') ++ show i
          _day    = p 4 y ++ '-':p 2 mo ++ '-':p 2 d
          _second = p 2 h ++ ':':p 2  m ++ ':':p 2 s
irc2notetaker _ _ _ = Nothing

