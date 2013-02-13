#!/usr/bin/env runhaskell
import Control.Monad (when)
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import Distribution.Verbosity
import System.Cmd (system)
import System.Directory (getModificationTime, doesFileExist)
import System.FilePath(combine)

main = defaultMainWithHooks hooks
hooks = simpleUserHooks { hookedPreProcessors = [trhsx] }

trhsx :: PPSuffixHandler
trhsx = ("fhs", f) where
    f buildInfo localBuildInfo = PreProcessor
     {platformIndependent = True
     ,runPreProcessor     = \ (inFileBaseDir,inFile) (outFileBaseDir,outFile) verbosity -> do
        when (verbosity >= verbose) $
            putStrLn ("checking that preprocessor is up-to-date")
        let [pIn, pOut] = ["Preprocessor/Hsx/Parser."++s | s <- ["ly","hs"]]
        exists <- doesFileExist pOut
        runHappy <- if not exists then return True else do
            [tIn, tOut] <- mapM getModificationTime [pIn, pOut]
            return (tIn > tOut)
        when runHappy $ system ("happy "++pIn) >> return ()
        system ("ghc --make Preprocessor/Main.hs -o preprocessor")

        when (verbosity >= verbose) $
            putStrLn ("preprocessing "++(combine inFileBaseDir inFile)++" to "++(combine outFileBaseDir outFile))
        writeFile (combine outFileBaseDir outFile) ("-- GENERATED file. Edit the ORIGINAL "++inFile++
                           " instead.\n")
        system ("./preprocessor "++(combine inFileBaseDir inFile)++" >> "++(combine outFileBaseDir outFile))
        return ()}

