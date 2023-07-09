--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import           Data.Monoid (mappend)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad (liftM)
import           Hakyll
import           Text.Pandoc as Pandoc
import           Text.Pandoc.Extensions
import           Text.Pandoc.Options
import           Text.Pandoc.Highlighting
import           Text.Pandoc.Templates (compileTemplate) 
import qualified Data.Map as M
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Functor.Identity (runIdentity)
import           Data.Ord (comparing)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Data.Function                   ((&))
import           System.FilePath                 ((</>))
--------------------------------------------------------------------------------
--- Code highlighting ---
-------------------------
syntaxHighlightingStyle :: Style
syntaxHighlightingStyle = pygments
-----------------------
--- Output location ---
-----------------------
config :: Configuration
config = defaultConfiguration
    { destinationDirectory = "docs"
    }
------------------------------------
--- All compilation happens here ---
------------------------------------
main :: IO ()
main = do 
    ------------------------------------
    -- Generate code for CSS styling ---
    ------------------------------------
    let css = styleToCss syntaxHighlightingStyle
    writeFile  ("css" </> "syntax.css") css >> putStrLn " Generated css/syntax.css"
    appendFile ("css" </> "syntax.css") "div.sourceCode { padding: 0.75em 0 0.75em 0; }" >> putStrLn " Updated padding for css/syntax.css"
    appendFile ("css" </> "syntax.css") "div.sourceCode { overflow: visible; }" >> putStrLn " Updated overflow for css/syntax.css"
    appendFile ("css" </> "syntax.css") "div.sourceCode { border-top: 0.18em dotted black; border-bottom: 0.18em dotted black }" 
        >> putStrLn " Added border to css/syntax.css"
    --------------------------------
    --- Construct and copy files ---
    --------------------------------
    hakyllWith config $ do 
        ------------------
        --- Copy files ---
        ------------------
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
        ---------------------------------------
        --- Compile contact and about pages ---
        ---------------------------------------
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
        ----------------------------
        --- Build tags for posts ---
        ----------------------------
        --- see: 1). https://javran.github.io/posts/2014-03-01-add-tags-to-your-hakyll-blog.html
        ---      2). https://github.com/kowainik/kowainik.github.io/blob/e3976e9eb686f6511258b3cea487af59f7c64bd6/templates/post.html
        postTags <- buildTags "posts/**/*" (fromCapture "tags/posts/*.html") 

        tagsRules postTags $ \tag pattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""
                title_link = "<a href='/archive.html'>Posts</a> tagged \"" ++ tag ++ "\""
            route idRoute
            compile $ do
                posts <- lastModifiedFirst =<< loadAll pattern
                let ids = map itemIdentifier posts
                tagsList <- sort . nub . concat <$> traverse getTags ids
                let ctx = postCtxWithTags tagsList
                        <> constField "title"      title
                        <> constField "title_link" title_link
                        <> listField  "posts" (postCtxWithTags tagsList) (return posts)
                        <> defaultContext 
                makeItem ""
                        >>= loadAndApplyTemplate "templates/tag.html" ctx
                        >>= loadAndApplyTemplate "templates/default.html" ctx
                        >>= relativizeUrls
        ----------------------------
        --- Build tags for notes ---
        ----------------------------
        notesTags <- buildTags "notes/**/*" (fromCapture "tags/notes/*.html") 

        tagsRules notesTags $ \tag pattern -> do
            let title = "Notes tagged \"" ++ tag ++ "\""
                title_link = "<a href='/archive.html'>Notes</a> tagged \"" ++ tag ++ "\""
            route idRoute
            compile $ do
                notes <- lastModifiedFirst =<< loadAll pattern
                let ids = map itemIdentifier notes
                tagsList <- sort . nub . concat <$> traverse getTags ids
                let ctx = postCtxWithTags tagsList
                        <> constField "title"      title
                        <> constField "title_link" title_link
                        <> listField  "posts" (postCtxWithTags tagsList) (return notes)
                        <> defaultContext 
                makeItem ""
                        >>= loadAndApplyTemplate "templates/tag.html" ctx
                        >>= loadAndApplyTemplate "templates/default.html" ctx
                        >>= relativizeUrls 
        ---------------------
        --- Compile posts ---
        ---------------------
        match "posts/**/*" $ do
            route $ setExtension "html"
            compile $ do
                i   <- myPandocBiblioCompiler
                tgs <- getTags (itemIdentifier i)
                let postTagsCtx = postCtxWithTags tgs
                loadAndApplyTemplate "templates/post.html" postTagsCtx i
                    >>= loadAndApplyTemplate "templates/post_base.html" postTagsCtx
                    >>= relativizeUrls
        ---------------------
        --- Compile notes ---
        ---------------------
        match "notes/**/*" $ do
            route $ setExtension "html"
            compile $ do 
                i   <- myPandocBiblioCompiler
                tgs <- getTags (itemIdentifier i)
                let postTagsCtx = postCtxWithTags tgs
                loadAndApplyTemplate "templates/note.html" postTagsCtx i
                    >>= loadAndApplyTemplate "templates/post_base.html" postTagsCtx
                    >>= relativizeUrls
        -----------------------
        --- Compile archive ---
        -----------------------
        --- see: https://github.com/kowainik/kowainik.github.io/blob/e3976e9eb686f6511258b3cea487af59f7c64bd6/templates/post.html
        create ["archive.html"] $ do
            route idRoute
            compile $ do
                engPosts <- lastModifiedFirst =<< loadAll "posts/eng/*"
                let engIds = map itemIdentifier engPosts
                engTags <- nub . concat <$> traverse getTags engIds
                
                mathPosts <- lastModifiedFirst =<< loadAll "posts/math/*"
                let mathIds = map itemIdentifier mathPosts
                mathTags <- nub . concat <$> traverse getTags mathIds
                
                miscPosts <- lastModifiedFirst =<< loadAll "posts/misc/*" 
                let miscIds = map itemIdentifier miscPosts
                miscTags <- nub . concat <$> traverse getTags miscIds
                
                let allTags = sort (engTags <> mathTags <> miscTags)
                let archiveCtx = 
                            listField  "tagsList"  (field "tag" $ pure . itemBody) (traverse makeItem allTags)
                        <>  listField  "engPosts"  (postCtxWithTags engTags)  (return engPosts)
                        <>  listField  "mathPosts" (postCtxWithTags mathTags) (return mathPosts)
                        <>  listField  "miscPosts" (postCtxWithTags miscTags) (return miscPosts)
                        <>  constField "title"     "Archives"
                        <>  defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/archive.html"      archiveCtx
                    >>= loadAndApplyTemplate "templates/archive_base.html" archiveCtx
                    >>= relativizeUrls
        -----------------------------
        --- Compile notes archives ---
        -----------------------------
        --- Books --- 
        create ["bookNotes.html"] $ do
            route idRoute
            compile $ do
                bookNotes <- lastModifiedFirst =<< loadAll "notes/book/*"
                let bookIds = map itemIdentifier bookNotes
                bookTags <- sort . nub . concat <$> traverse getTags bookIds
                let bookCtx =
                            listField  "tagsList"   (field "tag" $ pure . itemBody) (traverse makeItem bookTags)
                        <>  listField  "posts"      (postCtxWithTags bookTags) (return bookNotes)
                        <>  constField "title"      "Book Notes"
                        <>  constField "title_link" "Book <a href='archive.html'>Notes</a>"
                        <>  defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/notes-list.html" bookCtx
                    >>= loadAndApplyTemplate "templates/notes_base.html" bookCtx
                    >>= relativizeUrls
        --- Research ---
        create ["researchNotes.html"] $ do
            route idRoute
            compile $ do
                researchNotes <- lastModifiedFirst =<< loadAll "notes/research/*"
                let researchIds = map itemIdentifier researchNotes
                researchTags <- sort . nub . concat <$> traverse getTags researchIds
                let researchCtx =
                            listField  "tagsList"   (field "tag" $ pure . itemBody) (traverse makeItem researchTags)
                        <>  listField  "posts"      (postCtxWithTags researchTags) (return researchNotes)
                        <>  constField "title"      "Research Journal"
                        <>  constField "title_link" "Research <a href='archive.html'>Journal</a>"
                        <>  defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/notes-list.html" researchCtx
                    >>= loadAndApplyTemplate "templates/notes_base.html" researchCtx
                    >>= relativizeUrls
        --------------------------
        --- Compile index page ---
        --------------------------
        match "index.html" $ do
            route idRoute
            compile $ do
                posts <- lastModifiedFirst =<< loadAll "posts/**/*"
                let indexCtx =
                        listField "posts" postCtx (return posts) `mappend`
                        constField "title" "Home"                `mappend`
                        defaultContext
                getResourceBody
                    >>= applyAsTemplate indexCtx
                    >>= loadAndApplyTemplate "templates/index.html" indexCtx
                    >>= relativizeUrls
        ----------------------
        --- Copy templates ---
        ----------------------
        match "templates/*" $ compile templateCompiler
------------------------------------------------------------
--- Extra contexts (Collections of template $variables$) ---
------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"                `mappend`
    modificationTimeField "modDate" "%B %e, %Y" `mappend`
    defaultContext

--- See kowainik github ---
postCtxWithTags :: [String] -> Context String
postCtxWithTags tags = 
    listField "tagsList" (field "tag" $ pure . itemBody) (traverse makeItem tags)
    <> postCtx
----------------------------------
--- Citations helper functions ---
----------------------------------
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
-----------------------------
--- Sort by last-modified ---
-----------------------------
--- first modified to last --- 
-- FIX THIS: chronologicalByModification :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a] ---
chronologicalByModification =
        sortByM $ getItemModificationTime . itemIdentifier
    where
        sortByM :: (Monad m, Ord  k) => (a -> m k) -> [a] -> m [a]
        sortByM f xs = liftM (map fst . sortBy (comparing snd)) $
                       mapM (\x -> liftM (x,) (f x)) xs
--- Return last modified file first ---
-- FIX THIS: lastModifiedFirst :: (MonadMetadata m, MonadFail m) => [Item a] -> m [Item a] ---
lastModifiedFirst = liftM reverse . chronologicalByModification
-----------------------------------------------------
--- Custom compiler (extensions, math, citations) ---
-----------------------------------------------------
myPandocBiblioCompiler :: Compiler (Item String)
myPandocBiblioCompiler = do
    underlying <- getUnderlying
    toc        <- getMetadataField underlying "tableOfContents"
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
        writerOptions' = 
            defaultHakyllWriterOptions
            { writerExtensions = newExtensions
            , writerHTMLMathMethod = MathJax ""
            , writerHighlightStyle = Just syntaxHighlightingStyle
            }
        writerOptions = maybe 
            writerOptions'
            (const $ withToc writerOptions') toc
    csl <- load "chicago.csl"
    bib <- load "refs.bib" 
    getResourceBody >>= 
        turnOnLinkCitations defaultHakyllReaderOptions >>=
        processPandocBiblio csl bib >>= 
        return . writePandocWith writerOptions
-------------------------
--- Table of contents ---
-------------------------
--- see: https://svejcar.dev/posts/2019/11/27/table-of-contents-in-hakyll/
withToc :: WriterOptions -> WriterOptions
withToc options = options { writerNumberSections  = True
                          , writerTableOfContents = True
                          , writerTOCDepth        = 2
                          , writerTemplate        = Just tocTemplate
                          }

tocTemplate :: Pandoc.Template Text
tocTemplate = either error id . runIdentity . compileTemplate "" $ T.unlines
    [ "<br>" 
    , "<div class=\"toc\"><h3>Table of Contents</h3>"
    , "$toc$"
    , "</div>"
    , "<br>"
    , "<br>"
    , "$body$"
    ]
