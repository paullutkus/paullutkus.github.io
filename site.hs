--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Text.Pandoc as Pandoc
import           Text.Pandoc.Extensions
import           Text.Pandoc.Options
import           Text.Pandoc.Highlighting
import qualified Data.Map as M
import           Data.List
import           Data.Function                   ((&))
import           System.FilePath                 ((</>))


--------------------------------------------------------------------------------     
        
syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = pygments

config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    }

main :: IO ()
main = do 
    -- Generate css stytling for code highlighting
    let css = styleToCss syntaxHighlightingStyle
    writeFile  ("css" </> "syntax.css") css >> putStrLn " Generated css/syntax.css"
    appendFile ("css" </> "syntax.css") "div.sourceCode { padding: 0.75em 0 0.75em 0; }" >> putStrLn " Updated padding for css/syntax.css"
    appendFile ("css" </> "syntax.css") "div.sourceCode { overflow: visible; }" >> putStrLn " Updated overflow for css/syntax.css"
    appendFile ("css" </> "syntax.css") "div.sourceCode { border-top: 0.18em dotted black; border-bottom: 0.18em dotted black }" 
        >> putStrLn " Added border to css/syntax.css"

    hakyllWith config $ do 
        match "chicago.csl" $ do 
            route idRoute
            compile cslCompiler
    
        match "refs.bib" $ do 
            route idRoute
            compile biblioCompiler

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
     
        --match (fromList ["about.rst", "contact.markdown"]) $ do
        --    route   $ setExtension "html"
        --    compile $ pandocMathCompiler
        --        >>= loadAndApplyTemplate "templates/about_default.html" defaultContext
        --        >>= relativizeUrls

        match "about.rst" $ do
            route   $ setExtension "html"
            compile $ myPandocBiblioCompiler
                >>= loadAndApplyTemplate "templates/about_base.html" defaultContext
                >>= relativizeUrls
        
        match "contact.markdown" $ do
            route   $ setExtension "html"
            compile $ myPandocBiblioCompiler
                >>= loadAndApplyTemplate "templates/contact_base.html" defaultContext
                >>= relativizeUrls
        
        tags <- buildTags "posts/**/*" (fromCapture "tags/*.html")
        
        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""
                title_link = "<a href='/archive.html'>Posts</a> tagged \"" ++ tag ++ "\""
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ids = map itemIdentifier posts
                tagsList <- nub . concat <$> traverse getTags ids
                let ctx = postCtxWithTags tagsList
                        <> constField "title"      title
                        <> constField "title_link" title_link
                        <> listField  "posts" (postCtxWithTags tagsList) (return posts)
                        <> defaultContext
                
                makeItem ""
                        >>= loadAndApplyTemplate "templates/tag.html" ctx
                        >>= loadAndApplyTemplate "templates/default.html" ctx
                        >>= relativizeUrls 
        
        match "posts/**/*" $ do
            route $ setExtension "html"
            compile $ do
                i   <- myPandocBiblioCompiler
                tgs <- getTags (itemIdentifier i)
                let postTagsCtx = postCtxWithTags tgs
                loadAndApplyTemplate "templates/post.html" postTagsCtx i
                    >>= loadAndApplyTemplate "templates/post_base.html" postTagsCtx
                    >>= relativizeUrls
        
        create ["archive.html"] $ do
            route idRoute
            compile $ do
                --allPosts <- recentFirst =<< loadAll "posts/**/*"
                --let allIds = map itemIdentifier allPosts
                --allTags <- nub . concat <$> traverse getTags allIds

                engPosts <- recentFirst =<< loadAll "posts/eng/*"
                let engIds = map itemIdentifier engPosts
                engTags <- nub . concat <$> traverse getTags engIds
                
                mathPosts <- recentFirst =<< loadAll "posts/math/*"
                let mathIds = map itemIdentifier mathPosts
                mathTags <- nub . concat <$> traverse getTags mathIds
                
                miscPosts <- recentFirst =<< loadAll "posts/misc/*" 
                let miscIds = map itemIdentifier miscPosts
                miscTags <- nub . concat <$> traverse getTags miscIds
                               
                --allTags <- nub . concat <$> traverse getTags (engIds <> mathIds <> miscIds)
                
                --let archiveCtx = postCtxWithTags (engTags <> mathTags <> miscTags)
                let archiveCtx = 
                           listField  "tagsList"  (field "tag" $ pure . itemBody) (traverse makeItem (engTags <> mathTags <> miscTags))
                        <> listField  "engPosts"  (postCtxWithTags engTags)  (return engPosts)
                        <> listField  "mathPosts" (postCtxWithTags mathTags) (return mathPosts)
                        <> listField  "miscPosts" (postCtxWithTags miscTags) (return miscPosts)
                        <> constField "title"     "Archives"
                        <> defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                    >>= loadAndApplyTemplate "templates/archive_base.html" archiveCtx
                    >>= relativizeUrls
        
        
        --create ["archive.html"] $ compileArchive "Archive" "posts/**/*"
        
        -- DEFAULT TEST RUN (WORKS)
        --create ["archive.html"] $ compilePosts "Archive" "templates/archive.html" "posts/**/*" 

        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll "posts/**/*"
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

postCtxWithTags :: [String] -> Context String
postCtxWithTags tags = 
    listField "tagsList" (field "tag" $ pure . itemBody) (traverse makeItem tags)
    <> postCtx

{- Old
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx
-}

-- from github.com/kowainik/kowainik.github.io/

{- DEFAULT TEST RUN (WORKS)
compilePosts :: String -> Identifier -> Pattern -> Rules ()
compilePosts title page pat = do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll pat
        let ids = map itemIdentifier posts
        tagsList <- nub . concat <$> traverse getTags ids
        let ctx = postCtxWithTags tagsList
               <> constField "title" title
               <> listField "posts" postCtx (return posts)
               <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate page ctx
            >>= loadAndApplyTemplate "templates/archive_base.html" ctx
            >>= relativizeUrls
-}

{-
compileArchive :: String -> Pattern -> Rules ()
compileArchive title pat = do
    route idRoute
    compile $ do
        allPosts <- recentFirst =<< loadAll pat
        let allIds = map itemIdentifier allPosts
        allTags <- nub . concat <$> traverse getTags allIds 
        postsEng  <- recentFirst =<< loadAll "posts/eng/*"
        postsMath <- recentFirst =<< loadAll "posts/math/*"
        postsMisc <- recentFirst =<< loadAll "posts/misc/*" 
        let archiveCtx =  postCtxWithTags allTags
                <> listField  "postsEng"  postCtx (return postsEng)
                <> listField  "postsMath" postCtx (return postsMath)
                <> listField  "postsMisc" postCtx (return postsMisc)
                --listField  "tagsList"  (field "tag" $ pure . itemBody) (traverse makeItem allTags) `mappend`
                <> constField "title"     title
                <> defaultContext

        makeItem ""
            >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
            >>= loadAndApplyTemplate "templates/archive_base.html" archiveCtx
            >>= relativizeUrls
-}
 
turnOnLinkCitations :: ReaderOptions
                    -> Item String
                    -> Compiler (Item Pandoc)
turnOnLinkCitations ropt item = do
  pandoc <- readPandocWith ropt item
  withItemBody (return . addLinkCitations) pandoc

addLinkCitations :: Pandoc -> Pandoc
addLinkCitations (Pandoc meta a) =
  meta & unMeta
--     & M.insert "reference-section-title" (MetaString "<u>References</u>")
       & M.insert "link-citations" (MetaBool True)
       & \m -> Pandoc (Meta m) a

myPandocBiblioCompiler :: Compiler (Item String)
myPandocBiblioCompiler = do
    let markdownExtensions = 
            [ Ext_markdown_in_html_blocks 
            , Ext_bracketed_spans
            , Ext_citations
            , Ext_footnotes
            , Ext_raw_html
            ]
        mathExtensions = 
            [ Ext_tex_math_dollars
            , Ext_tex_math_double_backslash
            , Ext_latex_macros 
            , Ext_raw_tex
            ]
        codeExtensions = 
            [ Ext_fenced_code_blocks
            , Ext_backtick_code_blocks                      
            , Ext_fenced_code_attributes 
            ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions  
        newExtensions = foldr enableExtension defaultExtensions (markdownExtensions <> (mathExtensions <> codeExtensions))
        writerOptions = 
            defaultHakyllWriterOptions 
            { writerExtensions = newExtensions
            , writerHTMLMathMethod = MathJax ""
            , writerHighlightStyle = Just syntaxHighlightingStyle 
            }
    csl <- load "chicago.csl"
    bib <- load "refs.bib" 
    getResourceBody >>= 
        turnOnLinkCitations defaultHakyllReaderOptions >>=
        processPandocBiblio csl bib >>= 
        return . writePandocWith writerOptions
 
{-  
myPandocMathCompiler :: Compiler (Item String)
myPandocMathCompiler =
    let 
    markdownExtensions = 
        [ Ext_markdown_in_html_blocks 
        , Ext_bracketed_spans
        , Ext_citations
        , Ext_footnotes
        , Ext_raw_html
        ]
    mathExtensions = 
        [ Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros 
        , Ext_raw_tex
        ]
    codeExtensions = 
        [ Ext_fenced_code_blocks
        , Ext_backtick_code_blocks                      
        , Ext_fenced_code_attributes 
        ]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions  
    newExtensions = foldr enableExtension defaultExtensions (markdownExtensions <> (mathExtensions <> codeExtensions))
    writerOptions = 
        defaultHakyllWriterOptions 
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = MathJax ""
        , writerHighlightStyle = Just syntaxHighlightingStyle 
        }
    in pandocCompilerWith defaultHakyllReaderOptions writerOptions
-} 
