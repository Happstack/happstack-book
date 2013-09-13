module Main where

import Control.Applicative        ((<$>))
import Control.Monad              ((<=<))
import Data.List                  (intersperse, isSuffixOf)
import Development.Shake
import Development.Shake.FilePath
import SoHFilter (sohFilter)

chapters :: [FilePath]
chapters = [ "title.txt"
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
           , "Templates/HSX/WhatMore.lhs"
           , "Templates/HSX/I18n.cpp.lhs"
           , "Templates/Heist/TemplatesHeist.lhs"
           , "Templates/JMacro.lhs"
           , "RequestData/RqDataIntro.md"
           , "RequestData/HelloRqData.lhs"
           , "RequestData/RqDataPost.lhs"
           , "RequestData/RqDataUpload.lhs"
           , "RequestData/RqDataLimiting.lhs"
           , "RequestData/RqDataError.lhs"
--           , "RequestData/RqDataRead.lhs"
           , "RequestData/RqDataCheck.lhs"
           , "RequestData/RqDataCheckOther.lhs"
           , "RequestData/RqDataOptional.lhs"
           , "RequestData/Cookies.md"
           , "RequestData/CookieCounter.lhs"
           , "RequestData/CookieLife.lhs"
           , "RequestData/CookieIssues.lhs"
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
           , "AcidState/IxSetDataLens.lhs"
           , "AcidState/AcidStateAdvanced.lhs"
           , "Appendix/TemplateHaskell.lhs"
           ]

this           = "make2.hs"
allChapters    = "_build/allChapters.md"
allChaptersSoH = "_build/allChaptersSoH.md"
-- srclink        = "http://www.happstack.com/docs/book/src"

main :: IO ()
main = shakeArgs shakeOptions $ do
         let src  = map (\f -> "_build/src" </> (f -<.> "hs")) $ filter (isSuffixOf ".lhs") chapters
             tgts = ["_build/book.html", "_build/book.pdf", "_build/book.md", "_build/theme.css"] ++ src
         want tgts
         allChapters *> \out ->
             do need (this : chapters)
                let loadFile :: FilePath -> Action String
                    loadFile fp
                        | isSuffixOf ".cpp.lhs" fp =
                            do (Stdout r) <- command [] "cpphs" ["--noline",fp]
                               return r
                        | otherwise = readFile' fp
                allChaptersTxt <- (concat . intersperse "\n\n") <$> mapM loadFile chapters
                writeFileChanged allChapters allChaptersTxt
                system' "sed" ["-i", "s/%%%%/\\#\\#\\#\\#/", allChapters]
                system' "sed" ["-i", "s/%%%/\\#\\#\\#/", allChapters]
         "_build/theme.css" *> \out ->
             do need [this, "theme.css"]
                copyFile' "theme.css" out
         "_build/src//*.hs" *> \out ->
             do let inLhs = drop 11 $ out -<.> "lhs"
                need [this,inLhs]
                system' "sed" ["-n","s/^> \\?//w " ++ out, inLhs]
         "_build/book.html" *> \out ->
             do need [this,  allChapters]
--                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css","-o", out, allChapters]
--                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","http://happstack.com/docs/crashcourse/theme/theme.css","-o", out, allChapters]
                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","theme.css","-o", out, allChapters]
                system' "sed" ["-i","s/srclink/www\\.happstack\\.com\\/docs\\/crashcourse\\/new\\/src/", out]
         "_build/book.pdf" *> \out ->
             do need [this, allChapters]
                system' "pandoc" ["-V", "documentclass:book", "-f", "markdown+lhs","--latex-engine","pdflatex","--toc","--chapters","-o", out, allChapters]
         "_build/book.md" *> \out ->
             do need (this:"SoHFilter.hs":chapters)
                allChaptersTxt <- (concat . intersperse "\n\n") <$> mapM (sohFilter <=< readFile') chapters
                writeFileChanged out allChaptersTxt
                system' "sed" ["-i", "s/%%%%/\\#\\#\\#\\#/", out]
                system' "sed" ["-i", "s/%%%/\\#\\#\\#/", out]
                system' "sed" ["-i", "s/import Happstack.Server/import Happstack.Server.Env/", out]
                system' "sed" ["-i", "s/sourceCode literate haskell/haskell web active/", out]
--                system' "pandoc" ["-f", "markdown+lhs","-t","markdown_soh","-o", out, allChapters_]
         phony "publish" $
             do need tgts
                system' "rsync" ["-avxz", "--exclude","*.o","--exclude", "*.hi", "_build/", "jeremy@happstack.com:public_html/happstack-crashcourse/new/"]

--	rsync -avxz --exclude '*.o' --exclude '*.hi' html/ jeremy@happstack.com:public_html/happstack-crashcourse/

