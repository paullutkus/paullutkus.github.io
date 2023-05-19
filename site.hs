--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc.Extensions
import           Text.Pandoc.Options
import           Text.Pandoc.Highlighting
import           System.FilePath                 ((</>))


--------------------------------------------------------------------------------

syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = monochrome

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    }

main :: IO ()
main = do 
    -- Generate css stytling for code highlighting
    let css = styleToCss syntaxHighlightingStyle
    writeFile ("css" </> "syntax.css") css >> putStrLn " Generated css/syntax.css"


    hakyllWith config $ do 
            match "google9b53f6c193ef33e6.html" $ do
                route   idRoute
                compile copyFileCompiler

            match "images/*" $ do
                route   idRoute
                compile copyFileCompiler

            match "fonts/**/*" $ do
                route   idRoute
                compile copyFileCompiler

            match "css/*" $ do
                route   idRoute
                compile compressCssCompiler
         
            match (fromList ["about.rst", "contact.markdown"]) $ do
                route   $ setExtension "html"
                compile $ pandocMathCompiler
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

            match "posts/*" $ do
                route $ setExtension "html"
                compile $ pandocMathCompiler
                    >>= loadAndApplyTemplate "templates/post.html"    postCtx
                    >>= loadAndApplyTemplate "templates/default.html" postCtx
                    >>= relativizeUrls

            create ["archive.html"] $ do
                route idRoute
                compile $ do
                    posts <- recentFirst =<< loadAll "posts/*"
                    let archiveCtx =
                            listField "posts" postCtx (return posts) `mappend`
                            constField "title" "Archives"            `mappend`
                            defaultContext

                    makeItem ""
                        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                        >>= relativizeUrls

            match "index.html" $ do
                route idRoute
                compile $ do
                    posts <- recentFirst =<< loadAll "posts/*"
                    let indexCtx =
                            listField "posts" postCtx (return posts) `mappend`
                            constField "title" "Home"                `mappend`
                            defaultContext

                    getResourceBody
                        >>= applyAsTemplate indexCtx
                        >>= loadAndApplyTemplate "templates/index.html" indexCtx
                        >>= relativizeUrls

            match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let 
    mathExtensions = 
        [ Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros 
        ]
    codeExtensions = 
        [ Ext_fenced_code_blocks
        , Ext_backtick_code_blocks                      
        , Ext_fenced_code_attributes 
        ]
    newExtensions = foldr enableExtension defaultExtensions (mathExtensions <> codeExtensions)
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    writerOptions = 
        defaultHakyllWriterOptions 
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = MathJax ""
        , writerHighlightStyle = Just syntaxHighlightingStyle 
        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions
        
        
