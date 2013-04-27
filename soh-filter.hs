module Main where

import qualified Data.Set as Set
import System.Environment (getArgs)
import Text.Pandoc

main :: IO ()
main =
    do let readerOpts = def { readerExtensions = Set.fromList [ Ext_literate_haskell
                                                              , Ext_fenced_code_blocks
                                                              , Ext_backtick_code_blocks
                                                              ] }
           writerOpts = def { writerExtensions = Set.fromList [ Ext_fenced_code_attributes
                                                              , Ext_backtick_code_blocks
                                                              ] }
       [inFile] <- getArgs
       c <- readFile inFile
       putStr$ writeMarkdown writerOpts $ bottomUp srcFixups $ readMarkdown readerOpts c

srcFixups :: Pandoc -> Pandoc
srcFixups (Pandoc meta blocks) =
    let literateSrc = foldr addSrcChunk "" blocks
        blocks' = concatMap (addCompleteSrc literateSrc) blocks
    in Pandoc meta blocks'
    where
      addSrcChunk :: Block -> String -> String
      addSrcChunk (CodeBlock (_,cls,_) code) code'
          | "literate" `elem` cls = code' ++ code
      addSrcChunk _ code' = code'

      addCompleteSrc :: String -> Block -> [Block]
      addCompleteSrc code (CodeBlock (_,cls,_) code')
          | "source-goes-here" `elem` cls =
              [ Para [Str "The complete source code can be found below."]
              , CodeBlock ("", ["haskell active web"],[]) (rewriteImportHappstackServer code)
              ]
          | "haskell" `elem` cls =
              [CodeBlock ("", ["haskell"],[]) code']
      addCompleteSrc _code b = [b]

      rewriteImportHappstackServer :: String -> String
      rewriteImportHappstackServer = id