module Preprocessor.Hsx (
	  module Preprocessor.Hsx.Syntax
	, module Preprocessor.Hsx.Build
	, module Preprocessor.Hsx.Parser
	, module Preprocessor.Hsx.Pretty
	, module Preprocessor.Hsx.Transform
	, parseFileContents
	, parseFileContentsWithMode
	, parseFile
	) where

import Preprocessor.Hsx.Build
import Preprocessor.Hsx.Syntax
import Preprocessor.Hsx.Parser
import Preprocessor.Hsx.Pretty
import Preprocessor.Hsx.Transform

parseFile :: FilePath -> IO (ParseResult HsModule)
parseFile fp = readFile fp >>= (return . parseFileContentsWithMode (ParseMode fp))

parseFileContents :: String -> ParseResult HsModule
parseFileContents = parseFileContentsWithMode defaultParseMode

parseFileContentsWithMode :: ParseMode -> String -> ParseResult HsModule
parseFileContentsWithMode p rawStr =
	let cleanStr = unlines [ s | s@(c:_) <- lines rawStr, c /= '#' ]
	 in parseModuleWithMode p cleanStr
