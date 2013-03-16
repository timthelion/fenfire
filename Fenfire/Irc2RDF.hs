{-# OPTIONS_GHC -fglasgow-exts -fffi #-}
module Fenfire.Irc2RDF where

-- Irc2RDF: An IRC to SIOC RDF converter
-- Standalone compiling: 
--     ghc --make -o Irc2RDF -main-is Fenfire.Irc2RDF.main Fenfire/Irc2RDF.hs -lraptor
-- Usage with IRC protocol lines in real-time from stdin: 
--     Irc2RDF http://base-uri/ file-path/ .file-extension "channel1 channel2"
-- Usage with IRC protocol lines with prepended timestamps from stdin:
--     Irc2RDF -t http://base-uri/ file-path/ .file-extension "chan1 chan2"

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

import Fenfire.RDF (writeTriples, Triple, Node(..), LiteralTag(..), 
                    rdf_type, rdfs_label)

import System.Time (getClockTime, toUTCTime, CalendarTime(..), ClockTime(..), 
                    toClockTime)
import System.Environment (getArgs, getProgName)
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafeInterleaveIO)

import Data.Char (ord, toUpper, toLower, isLetter, isSpace)
import Data.Bits ((.&.))
import Data.List (isPrefixOf)
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


-- the vocabulary
dc_date           = IRI "http://purl.org/dc/elements/1.1/date"
dcterms_created   = IRI "http://purl.org/dc/terms/created"
xsd_dateTime      =     "http://www.w3.org/2001/XMLSchema#dateTime"
sioc_container_of = IRI "http://rdfs.org/sioc/ns#container_of"
sioc_has_creator  = IRI "http://rdfs.org/sioc/ns#has_creator"
sioc_content      = IRI "http://rdfs.org/sioc/ns#content"
sioc_Forum        = IRI "http://rdfs.org/sioc/ns#Forum"
sioc_Post         = IRI "http://rdfs.org/sioc/ns#Post"
sioc_User         = IRI "http://rdfs.org/sioc/ns#User"
ds_item           = IRI "http://fenfire.org/2007/03/discussion-summaries#item"
ds_occurrence     = 
    IRI "http://fenfire.org/2007/03/discussion-summaries#occurrence"

-- command line parsing and event dispatching
main = do (root,filepath,extension,channels,parseTimeStamps) <- do
              args <- getArgs
              case args of 
                  [root,filepath,extension,channels] ->
                      return (root,filepath,extension,channels,False)
                  ["-t",root,filepath,extension,channels] -> 
                      return (root,filepath,extension,channels,True)
                  _ -> error "[-t] root filepath extension channels"
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

          mapM_ (uncurry $ handle (words channels) root filepath extension) 
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

-- event handling by writing the results into files
handle :: [String] -> String -> FilePath -> String -> 
          String -> (ClockTime, Maybe Integer) -> IO ()
handle channels root filepath extension line (clockTime,offset) = do 
    let (file,output) = irc2rdf channels root filepath (clockTime,offset) line
    maybe (return ()) ((flip appendFile) (toUTF output).(++extension)) file

-- conversion from an irc protocol line to a fragment of turtle rdf
irc2rdf :: [String] -> String -> FilePath -> (ClockTime, Maybe Integer) -> 
           String -> (Maybe FilePath,String)
irc2rdf channels root filepath time s = (fp, writeTriples Map.empty ts) where
     (fp,ts) = uncurry (triples channels root filepath time) $ parse s

-- parsing of a line in irc protocol syntax
parse (':':rest) = (Just $ takeWhile (/=' ') rest,
                    parse' "" (tail $ dropWhile (/=' ') rest))
parse      rest  = (Nothing, parse' "" rest)

parse' acc       [] = [reverse acc]
parse' acc   ['\r'] = [reverse acc]
parse'  "" (':':xs) = [reverse . dropWhile (=='\r') $ reverse xs]
parse' acc (' ':xs) = reverse acc : parse' "" xs
parse' acc   (x:xs) = parse' (x:acc) xs

-- output file and a list of triples for a given irc event
triples :: [String] -> String -> FilePath -> (ClockTime, Maybe Integer) -> 
           Maybe String -> [String] -> (Maybe FilePath, [Triple])
triples channels root filepath (time,offset) (Just prefix) [cmd,target,msg0] 
    | map toUpper cmd == "PRIVMSG", 
      '#':channelName <- map toLower target, channelName `elem` channels,
      msg <- case msg0 of '+':cs -> cs; '-':cs -> cs; cs -> cs,
      not $ "[off]" `isPrefixOf` msg,
      not $ "\1ACTION [off]" `isPrefixOf` msg
    = 
    let file = channelName ++ "-" ++ day
        channel = IRI $ "irc://freenode/%23" ++ channelName
        uri = root ++ file ++ "#" ++ second ++ maybe "" (('.':) . show) offset
        event = IRI uri
        timestamp = Literal (day++"T"++second++"Z") (Type xsd_dateTime)
          
        item = IRI $ uri ++ "-item"

        concepts (c:cs) lbl | isLetter c = concepts cs (lbl++[c])
        concepts (':':':':cs) lbl = [(channel, ds_item, item),
                                     (item, rdfs_label, lbl'),
                                     (item, dc_date, timestamp),
                                     (item, ds_occurrence, event)] where
            lbl' = Literal (lbl ++ ": " ++ dropWhile isSpace cs) Plain
        concepts _ _ = []
    in
    (
    Just (filepath++file)
    ,
    [(channel, sioc_container_of, event),
     (channel, rdf_type, sioc_Forum),
     (event, dcterms_created, timestamp),
     (event, sioc_has_creator, creator),
     (event, sioc_content, Literal msg Plain),
     (event, rdfs_label, Literal ("<"++nick++"> "++msg) Plain),
     (event, rdf_type, sioc_Post),
     (creator, rdfs_label, Literal nick Plain),
     (creator, rdf_type, sioc_User)]
     ++ concepts msg ""
    )
    where 
          nick = takeWhile (/='!') prefix
          creator = IRI $ "irc://freenode/"++nick++",isuser"
          (CalendarTime y moe d h m s _ps _wd _yd _tzn _tz _isDST) 
              = toUTCTime time
          mo = (fromEnum moe+1)
          p n i = take (n-length (show i)) (repeat '0') ++ show i
          day    = p 4 y ++ '-':p 2 mo ++ '-':p 2 d
          second = p 2 h ++ ':':p 2  m ++ ':':p 2 s
triples _ _ _ _ _ _ = (Nothing, [])

