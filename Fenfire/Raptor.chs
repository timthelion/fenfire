-- We want the C compiler to always check that types match:
{-# LANGUAGE ForeignFunctionInterface #-}
module Fenfire.Raptor where

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

import Foreign (Ptr, FunPtr, IntPtr, Storable(pokeByteOff, peekByteOff), 
                allocaBytes, nullPtr, castPtr, freeHaskellFunPtr, malloc, peek)
import Foreign.C (CString, castCharToCChar, CFile,
                  CSize, CULong, CInt, CUInt, CUChar, CChar, peekCStringLen)
                  
import Data.ByteString (ByteString, useAsCStringLen, packCStringLen)

import System.Posix.IO (stdOutput)
import System.Posix.Types (Fd)
import System.Environment (getArgs)

import Control.Monad (when)
import Data.IORef (IORef, modifyIORef, readIORef, newIORef)
import Control.Exception (bracket)

import System.Glib.UTFString (withUTFString, withUTFStringLen, peekUTFString)

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

{#pointer *raptor_namespace as Namespace newtype#}

unNamespace :: Namespace -> Ptr Namespace
unNamespace (Namespace ptr) = ptr

{#pointer *raptor_locator as Locator newtype#}

unLocator :: Locator -> Ptr Locator
unLocator (Locator ptr) = ptr

{#pointer *parser as Parser newtype#}

unParser :: Parser -> Ptr Parser
unParser (Parser ptr) = ptr

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
                              str <- peekUTFString (castPtr cstr) 
                              return $ Uri str
          f v IDENTIFIER_TYPE_PREDICATE = f v IDENTIFIER_TYPE_RESOURCE
          f v IDENTIFIER_TYPE_LITERAL = peekUTFString v >>= return . Literal
          f v IDENTIFIER_TYPE_ANONYMOUS = peekUTFString v >>= return . Blank
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
                                       
getNamespace :: Namespace -> IO (Maybe String, Maybe String)
getNamespace ns = do
    prefixC <- {#call raptor_namespace_get_prefix#} ns
    prefixS <- if prefixC == nullPtr
                   then return Nothing
                   else fmap Just $ peekUTFString (castPtr prefixC)
    uri <- {#call raptor_namespace_get_uri#} ns
    uriC <- {#call uri_as_string#} (castPtr uri)
    uriS <- if uriC == nullPtr then return Nothing
                               else fmap Just $ peekUTFString (castPtr uriC)
    return (prefixS, uriS)

withURI :: String -> (Ptr URI -> IO a) -> IO a
withURI string = bracket (withUTFString string $ {# call new_uri #} . castPtr)
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
    withUTFString s $ \str -> do
        setValue t (castPtr str)
        io
withIdentifier setValue setFormat (Statement t) (Blank nodeID) io = do
    setFormat t (cFromEnum IDENTIFIER_TYPE_ANONYMOUS)
    withUTFString nodeID $ \str -> do
        setValue t (castPtr str)
        io

withSubject = withIdentifier {# set statement->subject #}
                             {# set statement->subject_type #} 

withPredicate = withIdentifier {# set statement->predicate #}
                               {# set statement->predicate_type #}

withObject = withIdentifier {# set statement->object #}
                            {# set statement->object_type #}

type StatementHandler a = Ptr a -> Statement -> IO ()
foreign import ccall "wrapper"
   mkStatementHandler :: (StatementHandler a) -> IO (FunPtr (StatementHandler a))

type NamespaceHandler a = Ptr a -> Namespace -> IO ()
foreign import ccall "wrapper"
   mkNamespaceHandler :: (NamespaceHandler a) -> IO (FunPtr (NamespaceHandler a))

type MessageHandler a = Ptr a -> Locator -> CString -> IO ()
foreign import ccall "wrapper"
   mkMessageHandler :: (MessageHandler a) -> IO (FunPtr (MessageHandler a))

foreign import ccall "raptor.h raptor_init" initRaptor :: IO ()
foreign import ccall "raptor.h raptor_uri_filename_to_uri_string" uri_filename_to_uri_string :: CString -> IO CString
foreign import ccall "raptor.h raptor_new_uri" new_uri :: Ptr CChar -> IO (Ptr URI)
foreign import ccall "raptor.h raptor_uri_copy" uri_copy :: Ptr URI -> IO (Ptr URI)

foreign import ccall "raptor.h raptor_print_statement_as_ntriples" print_statement_as_ntriples :: Statement -> Ptr CFile -> IO ()

foreign import ccall "stdio.h fdopen" fdopen :: Fd -> CString -> IO (Ptr CFile)
foreign import ccall "stdio.h fputc" fputc :: CChar -> Ptr CFile -> IO ()

foreign import ccall "string.h memset" c_memset :: Ptr a -> CInt -> CSize -> IO (Ptr a)


-- | Serialize the given triples into a file with the given filename
--
triplesToFilename :: [Triple] -> [(String, String)] -> String -> IO ()
triplesToFilename triples namespaces filename = do 
  initRaptor

  serializer <- withUTFString "turtle" {# call new_serializer #}
  when (unSerializer serializer == nullPtr) $ fail "serializer is null"
  
  withUTFString filename $ {# call serialize_start_to_filename #} serializer
  
  flip mapM_ namespaces $ \(prefixS, uriS) -> do
      withUTFString prefixS $ \prefixC -> withUTFString uriS $ \uriC -> do
          uri <- new_uri uriC
          {# call raptor_serialize_set_namespace #} serializer uri $ castPtr prefixC
          {# call free_uri #} uri

  allocaBytes {# sizeof statement #} $ \ptr -> do
    let t = Statement ptr
    flip mapM_ triples $ \(s,p,o) -> do
      c_memset ptr 0 {# sizeof statement #}
      withSubject t s $ withPredicate t p $ withObject t o $ do
        {# call serialize_statement #} serializer t
        return ()
  {# call serialize_end #} serializer
  {# call free_serializer #} serializer
  
-- | Serialize the given triples into memory
--
triplesToBytes :: [Triple] -> [(String, String)] -> String -> IO ByteString
triplesToBytes triples namespaces baseURI = do 
  initRaptor

  serializer <- withUTFString "turtle" {# call new_serializer #}
  when (unSerializer serializer == nullPtr) $ fail "serializer is null"
  
  result_str <- malloc
  result_len <- malloc
  
  base_uri <- withUTFString baseURI new_uri

  {# call serialize_start_to_string #} serializer base_uri result_str result_len
  
  flip mapM_ namespaces $ \(prefixS, uriS) -> do
      withUTFString prefixS $ \prefixC -> withUTFString uriS $ \uriC -> do
          uri <- new_uri uriC
          {# call raptor_serialize_set_namespace #} serializer uri $ castPtr prefixC
          {# call free_uri #} uri

  allocaBytes {# sizeof statement #} $ \ptr -> do
    let t = Statement ptr
    flip mapM_ triples $ \(s,p,o) -> do
      c_memset ptr 0 {# sizeof statement #}
      withSubject t s $ withPredicate t p $ withObject t o $ do
        {# call serialize_statement #} serializer t
        return ()
  {# call serialize_end #} serializer
  {# call free_serializer #} serializer
  
  result_str' <- fmap castPtr $ peek result_str
  result_len' <- fmap fromIntegral $ peek result_len
  result <- packCStringLen (result_str', result_len')

  {# call free_uri #} base_uri
  {# call free_memory #} (castPtr result_str')
  {# call free_memory #} (castPtr result_str)
  {# call free_memory #} (castPtr result_len)
  
  return result

filenameToURI :: String -> IO String
filenameToURI filename = do
  uri_str <- withUTFString filename uri_filename_to_uri_string
  r <- peekUTFString uri_str
  {# call free_memory #} (castPtr uri_str)
  return r  

-- | Parse a file with the given filename into triples
--
filenameToTriples :: String -> Maybe String -> IO ([Triple], [(String, String)])
filenameToTriples filename baseURI = do
  let suffix = reverse $ takeWhile (/= '.') $ reverse filename
      parsertype = case suffix of "turtle" -> "turtle"
                                  "ttl"    -> "turtle"
                                  "rdf"    -> "rdfxml"
                                  "rdfxml" -> "rdfxml"
                                  "nt"     -> "ntriples"
                                  _        -> "ntriples"

  initRaptor

  uri_str <- withUTFString filename uri_filename_to_uri_string
  uri <- new_uri uri_str
  base_uri <- maybe (uri_copy uri) (\s -> withUTFString s new_uri) baseURI

  result <- parse (\p -> {# call parse_file #} p uri base_uri) parsertype

  {# call free_uri #} uri
  {# call free_uri #} base_uri
  {# call free_memory #} (castPtr uri_str)
  
  return result
  
uriToTriples :: String -> Maybe String -> IO ([Triple], [(String, String)])
uriToTriples uri baseURI = do
  initRaptor

  uri' <- withUTFString uri new_uri
  base_uri <- maybe (uri_copy uri') (\s -> withUTFString s new_uri) baseURI
    
  result <- parse (\p -> {# call parse_uri #} p uri' base_uri) "guess"

  {# call free_uri #} uri'
  {# call free_uri #} base_uri
  
  return result
  
bytesToTriples :: String -> ByteString -> String -> IO ([Triple], [(String, String)])
bytesToTriples format bytes baseURI = do
  initRaptor

  base_uri <- withUTFString baseURI new_uri    
  result <- useAsCStringLen bytes $ \(cstr, len) ->
      parse (\p -> do 
          {# call start_parse #} p base_uri
          {# call parse_chunk #} p (castPtr cstr) (fromIntegral len) 1) format

  {# call free_uri #} base_uri
  
  return result

parse :: (Parser -> IO CInt) -> String -> IO ([Triple], [(String, String)])
parse fn parsertype = do
  triples <- newIORef []
  namespaces <- newIORef []

  rdf_parser <- withUTFString parsertype {# call new_parser #}
  when (unParser rdf_parser == nullPtr) $ fail "parser is null"

  stHandler <- mkStatementHandler $ \_user_data triple -> do
    s <- getSubject triple
    p <- getPredicate triple
    o <- getObject triple
    modifyIORef triples ((s,p,o):)

  nsHandler <- mkNamespaceHandler $ \_user_data ns -> do
    (prefix, uri') <- getNamespace ns
    case (prefix, uri') of 
        (Just prefix',Just uri'') -> modifyIORef namespaces ((prefix', uri''):)
        _                         -> return ()

  let msgHandler intro = mkMessageHandler $ \_user_data locator msg -> do
        size <- {# call format_locator #} nullPtr 0 locator
        when (size > 0) $ allocaBytes (cIntConv size) $ \ptr -> do
          size' <- {# call format_locator #} ptr (cIntConv size) locator
          when (size' == 0) $ peekUTFString ptr >>= putStr
        putStr intro
        peekUTFString msg >>= putStrLn

  fatalHandler   <- msgHandler " parser fatal error - "
  errorHandler   <- msgHandler " parser error - "
  warningHandler <- msgHandler " parser warning - "

  {# call set_statement_handler #} rdf_parser nullPtr stHandler
  {# call set_namespace_handler #} rdf_parser nullPtr nsHandler
  {# call set_fatal_error_handler #} rdf_parser nullPtr fatalHandler
  {# call set_error_handler #} rdf_parser nullPtr errorHandler
  {# call set_warning_handler #} rdf_parser nullPtr warningHandler

  fn rdf_parser

  {# call free_parser #} rdf_parser
  freeHaskellFunPtr stHandler
  freeHaskellFunPtr nsHandler
  freeHaskellFunPtr fatalHandler
  freeHaskellFunPtr errorHandler
  freeHaskellFunPtr warningHandler
  
  t <- readIORef triples; n <- readIORef namespaces
  return (t, n)

-- The following print_triple and filenameToStdout are an incomplete and 
-- improved translation of raptor examples/rdfprint.c:

print_triple :: Ptr CFile -> StatementHandler a
print_triple outfile _user_data s = do print_statement_as_ntriples s outfile
                                       fputc (castCharToCChar '\n') outfile

filenameToStdout :: String -> IO ()
filenameToStdout filename = do
  outfile <- withUTFString "w" $ fdopen stdOutput

  initRaptor
  rdf_parser <- withUTFString "guess" {# call new_parser #}
  when (unParser rdf_parser == nullPtr) $ fail "parser is null"
  mkStatementHandler (print_triple outfile) >>= {# call set_statement_handler #} rdf_parser nullPtr
  uri <- withUTFString filename uri_filename_to_uri_string >>= new_uri
  base_uri <- uri_copy uri
  {# call parse_file #} rdf_parser uri base_uri
  return ()
