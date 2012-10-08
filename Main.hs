module Main where

import Control.Applicative ((<$>))
import Development.Shake
import Development.Shake.FilePath

chapters :: [FilePath]
chapters = [ "title.txt"
           , "HelloWorld.lhs"
           , "MonadPlus.lhs"
           , "RouteFiltersIntro.md"
           , "Dir.lhs"
           , "Dir2.lhs"
           , "Dirs.lhs"
           , "Path.lhs"
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
