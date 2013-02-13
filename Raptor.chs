-- We want the C compiler to always check that types match:
{-# LANGUAGE ForeignFunctionInterface #-}
module Raptor where

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

import Foreign (Ptr, FunPtr, Storable(pokeByteOff, peekByteOff), allocaBytes,
                nullPtr, castPtr, freeHaskellFunPtr)
import Foreign.C (CString, castCharToCChar, withCString, peekCString, CFile,
                  CSize, CInt, CUChar, CChar)

import System.Posix.IO (stdOutput)
import System.Posix.Types (Fd)
import System.Environment (getArgs)

import Control.Monad (when)
import Data.IORef (modifyIORef, readIORef, newIORef)
import Control.Exception (bracket)

#include <raptor.h>

-- the following three helpers are copied from C2HS.hs:
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = cIntConv . fromEnum

cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral


{#context lib="raptor" prefix="raptor"#}

{#enum raptor_identifier_type as IdType {} deriving (Show)#}

{#enum raptor_uri_source as UriSource {} deriving (Show)#}

{#pointer raptor_uri as URI newtype#}

{#pointer *statement as Statement newtype#}

unStatement :: Statement -> Ptr Statement
unStatement (Statement ptr) = ptr

{#pointer *parser as Parser newtype#}

{#pointer *serializer as Serializer newtype#}

unSerializer :: Serializer -> Ptr Serializer
unSerializer (Serializer ptr) = ptr

type Triple = (Identifier, Identifier, Identifier)

data Identifier = Uri String | Blank String | Literal String
                  deriving (Show)

mkIdentifier :: IO (Ptr ()) -> IO CInt -> IO Identifier
mkIdentifier value format = do
  value' <- value
  format' <- format
  f (castPtr value') (cToEnum format')
    where f v IDENTIFIER_TYPE_RESOURCE = do
                              cstr <- {#call uri_as_string#} (castPtr v) 
                              str <- peekCString (castPtr cstr) 
                              return $ Uri str
          f v IDENTIFIER_TYPE_PREDICATE = f v IDENTIFIER_TYPE_RESOURCE
          f v IDENTIFIER_TYPE_LITERAL = peekCString v >>= return . Literal
          f v IDENTIFIER_TYPE_ANONYMOUS = peekCString v >>= return . Blank
          f _ i = error $ "Raptor.mkIdentifier: Deprecated type: " ++ show i

getSubject :: Statement -> IO Identifier
getSubject (Statement s) = mkIdentifier ({#get statement->subject#} s)
                                        ({#get statement->subject_type#} s)

getPredicate :: Statement -> IO Identifier
getPredicate (Statement s) = mkIdentifier ({#get statement->predicate#} s)
                                          ({#get statement->predicate_type#} s)

getObject :: Statement -> IO Identifier
getObject (Statement s) = mkIdentifier ({#get statement->object#} s)
                                       ({#get statement->object_type#} s)

withURI :: String -> (Ptr URI -> IO a) -> IO a
withURI string = bracket (withCString string $ {# call new_uri #} . castPtr)
                         {# call free_uri #}

withIdentifier :: (Ptr Statement -> Ptr () -> IO ()) ->
                  (Ptr Statement -> CInt -> IO ()) -> 
                  Statement -> Identifier -> IO a -> IO a
withIdentifier setValue setFormat (Statement t) (Uri s) io = do 
    setFormat t (cFromEnum IDENTIFIER_TYPE_RESOURCE)
    withURI s $ \uri -> do
        setValue t (castPtr uri)
        io
withIdentifier setValue setFormat (Statement t) (Literal s) io = do
    setFormat t (cFromEnum IDENTIFIER_TYPE_LITERAL)
    withCString s $ \str -> do
        setValue t (castPtr str)
        io
withIdentifier _ _ _ i _ =
    error $ "Raptor.setIdentifier: unimplemented: " ++ show i

withSubject = withIdentifier {# set statement->subject #}
                             {# set statement->subject_type #} 

withPredicate = withIdentifier {# set statement->predicate #}
                               {# set statement->predicate_type #}

withObject = withIdentifier {# set statement->object #}
                            {# set statement->object_type #}

type Handler a = Ptr a -> Statement -> IO ()
foreign import ccall "wrapper"
   mkHandler :: (Handler a) -> IO (FunPtr (Handler a))

foreign import ccall "raptor.h raptor_init" initRaptor :: IO ()
foreign import ccall "raptor.h raptor_new_parser" new_parser :: Ptr CChar -> IO (Ptr Parser)
foreign import ccall "raptor.h raptor_set_statement_handler" set_statement_handler :: Ptr Parser -> Ptr a -> FunPtr (Handler a) -> IO () 
foreign import ccall "raptor.h raptor_uri_filename_to_uri_string" uri_filename_to_uri_string :: CString -> IO CString
foreign import ccall "raptor.h raptor_new_uri" new_uri :: Ptr CChar -> IO (Ptr URI)
foreign import ccall "raptor.h raptor_uri_copy" uri_copy :: Ptr URI -> IO (Ptr URI)
foreign import ccall "raptor.h raptor_parse_file" parse_file :: Ptr Parser -> Ptr URI -> Ptr URI -> IO ()

foreign import ccall "raptor.h raptor_print_statement_as_ntriples" print_statement_as_ntriples :: Statement -> Ptr CFile -> IO ()

foreign import ccall "stdio.h fdopen" fdopen :: Fd -> CString -> IO (Ptr CFile)
foreign import ccall "stdio.h fputc" fputc :: CChar -> Ptr CFile -> IO ()

foreign import ccall "string.h memset" c_memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)


-- | Serialize the given triples into a file with the given filename
--
triplesToFilename :: [Triple] -> String -> IO ()
triplesToFilename triples filename = do 
  initRaptor

  serializer <- withCString "ntriples" {# call new_serializer #}
  when (unSerializer serializer == nullPtr) $ fail "serializer is null"
  
  withCString filename $ {# call serialize_start_to_filename #} serializer

  allocaBytes {# sizeof statement #} $ \ptr -> do
    let t = Statement ptr
    flip mapM_ triples $ \(s,p,o) -> do
      c_memset ptr 0 {# sizeof statement #}
      withSubject t s $ withPredicate t p $ withObject t o $ do
        {# call serialize_statement #} serializer t
        return ()
  {# call serialize_end #} serializer
  {# call free_serializer #} serializer
  {# call finish #}

-- | Parse a file with the given filename into triples
--
filenameToTriples :: String -> IO [Triple]
filenameToTriples filename = do 
  result <- newIORef []

  initRaptor
  let suffix = reverse $ takeWhile (/= '.') $ reverse filename
      parsertype = case suffix of "turtle" -> "turtle"
                                  _        -> "guess"
  rdf_parser <- withCString parsertype new_parser 
  when (rdf_parser == nullPtr) $ fail "parser is null"
  handler <- mkHandler $ \_user_data triple -> do
    s <- getSubject triple
    p <- getPredicate triple
    o <- getObject triple
    modifyIORef result ((s,p,o):)

  set_statement_handler rdf_parser nullPtr handler
  uri_str <- withCString filename uri_filename_to_uri_string
  uri <- new_uri uri_str
  base_uri <- uri_copy uri
  parse_file rdf_parser uri base_uri

  {# call free_parser #} (Parser rdf_parser)
  freeHaskellFunPtr handler
  {# call free_uri #} uri
  {# call free_uri #} base_uri
  {# call free_memory #} (castPtr uri_str)

  {# call finish #}
  readIORef result

-- The following print_triple and filenameToStdout are an incomplete and 
-- improved translation of raptor examples/rdfprint.c:

print_triple :: Ptr CFile -> Handler a
print_triple outfile _user_data s = do print_statement_as_ntriples s outfile
                                       fputc (castCharToCChar '\n') outfile

filenameToStdout :: String -> IO ()
filenameToStdout filename = do
  outfile <- withCString "w" $ fdopen stdOutput

  initRaptor
  rdf_parser <- withCString "guess" new_parser 
  when (rdf_parser == nullPtr) $ fail "parser is null"
  mkHandler (print_triple outfile) >>= set_statement_handler rdf_parser nullPtr
  uri <- withCString filename uri_filename_to_uri_string >>= new_uri
  base_uri <- uri_copy uri
  parse_file rdf_parser uri base_uri
  return ()
