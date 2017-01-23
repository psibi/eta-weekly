--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend, (<>))
import Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main =
  hakyll $
  do match "images/*" $
       do route idRoute
          compile copyFileCompiler
     match "images/posts/*" $
       do route idRoute
          compile copyFileCompiler
     match "css/*" $
       do route idRoute
          compile compressCssCompiler
     match (fromList ["about.md", "contact.markdown"]) $
       do route $ setExtension "html"
          compile $
            pandocCompiler >>= loadAndApplyTemplate "templates/default.html" defaultContext >>=
            relativizeUrls
     match "posts/*" $
       do route $ setExtension "html"
          compile $
            pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx >>=
            relativizeUrls
     create ["archive.html"] $
       do route idRoute
          compile $
            do posts <- recentFirst =<< loadAll "posts/*"
               let archiveCtx =
                     listField "posts" postCtx (return posts) `mappend`
                     constField "title" "Archives" `mappend`
                     defaultContext
               makeItem "" >>=
                 loadAndApplyTemplate "templates/archive_base.html" archiveCtx >>=
                 relativizeUrls
     create ["index.html"] $
       do route idRoute
          compile $
            do posts <- recentFirst =<< loadAll "posts/*"
               let sixPosts = take 5 posts
               let archiveCtx =
                     listField "posts" postCtx (return sixPosts) `mappend`
                     constField "title" "Home" `mappend`
                     postCtx
               makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
                 loadAndApplyTemplate "templates/default.html" archiveCtx >>=
                 relativizeUrls
     match "templates/*" $ compile templateCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" `mappend` defaultContext
