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
           ]

allChapters = "_build/allChapters.txt"

main :: IO ()
main = shake shakeOptions $ do
         want ["_build/book.html", "_build/book.pdf"]
         allChapters *> \out ->
             do need ("Main.hs" : chapters )
                allChaptersTxt <- concat <$> mapM readFile' chapters
                writeFileChanged allChapters allChaptersTxt
         "_build/*.html" *> \out ->
             do need [allChapters]
                system' "pandoc" ["-f", "markdown+lhs","-t","html","-s","--toc","--chapters","-o", out, allChapters]
         "_build/*.pdf" *> \out ->
             do need [allChapters]
                system' "pandoc" ["-f", "markdown+lhs","--latex-engine","pdflatex","--toc","--chapters","-o", out, allChapters]
