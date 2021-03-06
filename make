#!/usr/bin/runhaskell
module Main where

import Control.Applicative ((<$>))
import Development.Shake
import Development.Shake.FilePath

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
{-
           , "Templates/TemplatesIntro.md"
           , "Templates/HelloBlaze.lhs"
           , "Templates/TemplatesHSP.lhs"
           , "Templates/TemplatesHSPI18n.lhs"
           , "Templates/TemplatesHeist.lhs"
           , "Templates/JMacro.lhs"
           , "RequestData/RqDataIntro.md"
           , "RequestData/HelloRqData.lhs"
           , "RequestData/RqDataPost.lhs"
           , "RequestData/RqDataUpload.lhs"
           , "RequestData/RqDataLimiting.lhs"
           , "RequestData/RqDataError.lhs"
           , "RequestData/RqDataRead.lhs"
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
-}
           ]

allChapters = "_build/allChapters.md"

main :: IO ()
main = shake shakeOptions $ do
         want ["_build/book.html", "_build/book.pdf", "_build/book.md"]
         allChapters *> \out ->
             do need ("make" : chapters )
                allChaptersTxt <- concat <$> mapM readFile' chapters
                writeFileChanged allChapters allChaptersTxt
                system' "sed" ["-i", "s/%%%%/\\#\\#\\#\\#/", allChapters]
                system' "sed" ["-i", "s/%%%/\\#\\#\\#/", allChapters]
         "_build/book.html" *> \out ->
             do need ["make",  allChapters]
                system' "pandoc" ["-f", "markdown+lhs","-t","html5","-s","--toc","--chapters","--css","http://netdna.bootstrapcdn.com/twitter-bootstrap/2.3.1/css/bootstrap-combined.min.css","-o", out, allChapters]
         "_build/book.pdf" *> \out ->
             do need ["make", allChapters]
                system' "pandoc" ["-V", "documentclass:book", "-f", "markdown+lhs","--latex-engine","pdflatex","--toc","--chapters","-o", out, allChapters]
         "_build/book.md" *> \out ->
             do need ["make",  allChapters]
                let allChapters_ = (allChapters++"_")
                system' "cp" [allChapters, allChapters_]
                system' "sed" ["-i", "s/import Happstack.Server/import Happstack.Server.Env/", allChapters_]
                system' "pandoc" ["-f", "markdown+lhs","-t","markdown_soh","-o", out, allChapters_]
                system' "sed" ["-i", "s/sourceCode literate haskell/haskell web active/", out]
