module Main where

import Control.Applicative        ((<$>))
import Control.Monad              ((<=<), mzero)
import Data.List                  (intersperse, isSuffixOf)
import Development.Shake
import Development.Shake.FilePath
-- import SoHFilter (FixupMode(Consolidate, Remove), sohFilter)
import System.Exit (ExitCode(..))
import Prelude hiding ((*>))

system' = cmd_

chapters :: [FilePath]
chapters = [ "title.txt"
           , "Intro.md"
           , "HelloWorld.lhs"
           , "RouteFilters/MonadPlus.lhs"
           , "RouteFilters/RouteFiltersIntro.md"
           , "RouteFilters/Dir.lhs"
           , "RouteFilters/Dir2.lhs"
           , "RouteFilters/Dirs.lhs"
           , "RouteFilters/Path.lhs"
           , "RouteFilters/FromReqURI.lhs"
           , "RouteFilters/Method.lhs"
           , "RouteFilters/MatchMethod.lhs"
           , "RouteFilters/OtherRouteFilters.md"
           , "Templates/TemplatesIntro.md"
           , "Templates/HelloBlaze.lhs"
           , "Templates/HSX/hsx2hs.lhs"
           , "Templates/HSX/hsx-qq.lhs"
           , "Templates/HSX/What.lhs"
           , "Templates/HSX/WhatMore.md"
           , "Templates/HSX/I18n.cpp.lhs"
--           , "Templates/Heist/TemplatesHeist.lhs"
           , "Templates/JMacro.lhs"
           , "RequestData/RqDataIntro.md"
           , "RequestData/HelloRqData.lhs"
           , "RequestData/RqDataPost.lhs"
           , "RequestData/RqDataUpload.lhs"
           , "RequestData/RqDataLimiting.md"
           , "RequestData/RqDataError.lhs"
--           , "RequestData/RqDataRead.lhs"
           , "RequestData/RqDataCheck.lhs"
           , "RequestData/RqDataCheckOther.lhs"
           , "RequestData/RqDataOptional.lhs"
           , "RequestData/Cookies.md"
           , "RequestData/CookieCounter.lhs"
           , "RequestData/CookieLife.md"
           , "RequestData/CookieIssues.md"
           , "RequestData/CookieFeatures.md"
           , "FileServing/FileServing.md"
           , "FileServing/FileServingDirectory.lhs"
           , "FileServing/FileServingSingle.lhs"
           , "FileServing/FileServingAdvanced.md"
           , "Reform/Reform.lhs"
           , "WebRoutes/WebRoutesIntro.md"
           , "WebRoutes/WebRoutesDemo.lhs"
           , "WebRoutes/WebRoutesBoomerang.lhs"
           , "WebRoutes/WebRoutesHSP.lhs"
           , "AcidState/AcidStateIntro.md"
           , "AcidState/AcidStateCounter.lhs"
           , "AcidState/IxSet.lhs"
--           , "AcidState/IxSetDataLens.lhs"
           , "AcidState/AcidStateAdvanced.lhs"
           , "Appendix/TemplateHaskell.md"
           ]

these           :: [FilePath]
these           = ["make2.hs","SoHFilter.hs"]
allChapters    = "_build/allChapters.md"
allChaptersSoH = "_build/allChaptersSoH.md"
-- srclink        = "http://www.happstack.com/docs/book/src"

main :: IO ()
main = shakeArgs shakeOptions $ do
         let src  = map (\f -> "_build/src" </> (f -<.> "hs")) $ filter (isSuffixOf ".lhs") chapters
             tgts = ["_build/happstack-book.html", "_build/happstack-book.pdf", "_build/happstack-book.epub", {- "_build/happstack-book.mobi", -} {- "_build/happstack-book.md", -} "_build/theme.css", "_build/src/messages.zip"] ++ src
         want tgts
         allChapters *> \out ->
             do need (these ++ chapters)
                let loadFile :: FilePath -> Action String
                    loadFile fp
                        | isSuffixOf ".cpp.lhs" fp =
                            do (Stdout r) <- command [] "cpphs" ["--noline",fp]
                               return r
--                               sohFilter Remove r
                        | isSuffixOf ".lhs" fp =
                            do c <- readFile' fp
--                               sohFilter Remove c
                               return c
                        | otherwise = readFile' fp
--                allChaptersTxt <- (concat . intersperse "\n\n") <$> mapM (loadFile) chapters
                allChaptersTxt <- (concat . intersperse "\n\n") <$> mapM loadFile chapters
                writeFileChanged allChapters allChaptersTxt
                system' "sed" ["-i", "s/%%%%/\\#\\#\\#\\#/", allChapters]
                system' "sed" ["-i", "s/%%%/\\#\\#\\#/", allChapters]
         "_build/theme.css" *> \out ->
             do need $ "theme.css" : these
                copyFile' "theme.css" out
         "_build/src//*.hs" *> \out ->
             do let inLhs = drop 11 $ out -<.> "lhs"
                need $ inLhs : these
                system' "sed" ["-n","s/^> \\?//w " ++ out, inLhs]
         "_build/happstack-book.html" *> \out ->
             do need $ allChapters : these
--                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css","-o", out, allChapters]
--                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","http://happstack.com/docs/crashcourse/theme/theme.css","-o", out, allChapters]
                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--top-level-division","chapter","--css","theme.css","-o", out, allChapters]
                system' "sed" ["-i","s/srclink/www\\.happstack\\.com\\/docs\\/crashcourse\\/src/g", out]
                system' "ln"  ["-s", "-f", "happstack-book.html", "_build/index.html"]
         "_build/happstack-book.pdf" *> \out ->
             do need $ allChapters:these
                system' "pandoc" ["-V", "documentclass:book", "-f", "markdown+lhs","--pdf-engine","pdflatex","--toc","--top-level-division=chapter","-o", out, allChapters]
         "_build/happstack-book.epub" *> \out ->
             do need $ allChapters:these
                system' "pandoc" ["-f", "markdown+lhs","-o", out, allChapters]
         "_build/happstack-book.mobi" *> \out ->
             do need $ "_build/happstack-book.epub":these
                (Exit c) <- command [] "kindlegen" ["_build/happstack-book.epub"]
                if (c == ExitSuccess) || (c == ExitFailure 1)
                   then return ()
                   else error $ "kindlgen failed with exit code: " ++ show c
{-
         "_build/happstack-book.md" *> \out ->
             do need $ chapters++these
                     -- FIXME: probably needs to call loadFile
                allChaptersTxt <- (concat . intersperse "\n\n") <$> mapM ((sohFilter Consolidate) <=< readFile') chapters
                writeFileChanged out allChaptersTxt
                system' "sed" ["-i", "s/%%%%/\\#\\#\\#\\#/", out]
                system' "sed" ["-i", "s/%%%/\\#\\#\\#/", out]
                system' "sed" ["-i", "s/import Happstack.Server/import Happstack.Server.Env/", out]
                system' "sed" ["-i", "s/sourceCode literate haskell/haskell web active/", out]
--                system' "pandoc" ["-f", "markdown+lhs","-t","markdown_soh","-o", out, allChapters_]
-}
         "_build/src/messages.zip" *> \out ->
             do msgs <- getDirectoryFiles "." ["messages//*.msg"]
                need msgs
                system' "zip" (out:msgs)
         phony "check-demos" $
               do need src
                  mapM_ (\hs -> system' "ghc" ["-fno-code", hs]) src
         phony "publish" $
             do need tgts
                system' "rsync" ["-avxz", "--exclude","*.o","--exclude", "*.hi", "_build/", "jeremy@happstack.com:/home/jeremy/public_html/happstack-crashcourse/"]

--	rsync -avxz --exclude '*.o' --exclude '*.hi' html/ jeremy@happstack.com:public_html/happstack-crashcourse/

