module SoHFilter where

import Control.Monad.Trans (MonadIO(liftIO))
import qualified Data.Set  as Set
import System.Environment  (getArgs)
import Text.Pandoc

sohMain :: IO ()
sohMain =
    do [inFile] <- getArgs
       contents <- readFile inFile
       putStr =<< sohFilter contents

sohFilter :: (MonadIO m ) => String -> m String
sohFilter contents = liftIO $
    do let readerOpts = def { readerExtensions = Set.fromList [ Ext_literate_haskell
                                                              , Ext_fenced_code_blocks
                                                              , Ext_fenced_code_attributes
                                                              , Ext_backtick_code_blocks
                                                              , Ext_pandoc_title_block
                                                              ] }
           writerOpts = def { writerExtensions = Set.fromList [ Ext_fenced_code_attributes
                                                              , Ext_backtick_code_blocks
                                                              ] }

       return $ writeMarkdown writerOpts $ bottomUp srcFixups $ readMarkdown readerOpts contents

{-

If the file contains an element with 'source-goes-here' then collect all the literate blocks and put them there and mark the block as 'haskell web active'.

If there are no source-goes-here blocks then mark each literate block as 'haskell web active'.

-}
srcFixups :: Pandoc -> Pandoc
srcFixups (Pandoc meta blocks) =
    let (literateSrc, sgh) = foldr addSrcChunk ("", False) blocks
        blocks' =
            if sgh
               then concatMap (addCompleteSrc literateSrc) blocks
               else map markLiterateActive blocks
    in Pandoc meta blocks'
    where
      addSrcChunk :: Block -> (String, Bool) -> (String, Bool)
      addSrcChunk (CodeBlock (_,cls,_) code) (code', sgh)
          | "literate" `elem` cls         = (code ++ code', sgh)
          | "source-goes-here" `elem` cls = (code ++ code', True)
      addSrcChunk _ a = a

      addCompleteSrc :: String -> Block -> [Block]
      addCompleteSrc code (CodeBlock (_,cls,_) code')
          | "source-goes-here" `elem` cls =
              [ Para [Str "The complete source code can be found below."]
              , CodeBlock ("", ["haskell active web"],[]) (rewriteImportHappstackServer code)
              ]
          | "haskell" `elem` cls =
              [CodeBlock ("", ["haskell"],[]) code']
      addCompleteSrc _code b = [b]

      markLiterateActive :: Block -> Block
      markLiterateActive (CodeBlock (_,cls,_) code)
          | "literate" `elem` cls =
              CodeBlock ("", ["haskell active web"],[]) (rewriteImportHappstackServer code)
      markLiterateActive b = b

      rewriteImportHappstackServer :: String -> String
      rewriteImportHappstackServer = id