{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid ((<>))
import Hakyll

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
     match "posts/*" $
       do route $ setExtension "html"
          compile $
            pandocCompiler >>= loadAndApplyTemplate "templates/post.html" postCtx >>=
            saveSnapshot "content" >>=
            relativizeUrls
     create ["archive.html"] $
       do route idRoute
          compile $
            do posts <- recentFirst =<< loadAll "posts/*"
               let archiveCtx =
                     listField "posts" postCtx (return posts) <>
                     constField "title" "Archives" <>
                     defaultContext
               makeItem "" >>= loadAndApplyTemplate "templates/archive.html" archiveCtx >>=
                 relativizeUrls
     create ["index.html"] $
       do route idRoute
          compile $
            do posts <- recentFirst =<< loadAll "posts/*"
               let sixPosts = take 5 posts
               let archiveCtx =
                     listField "posts" postCtx (return sixPosts) <>
                     boolField "index" (\_ -> True) <>
                     constField "title" "Home" <>
                     postCtx
               makeItem "" >>= loadAndApplyTemplate "templates/default.html" archiveCtx >>=
                 relativizeUrls
     create ["atom.xml"] $
       do route idRoute
          compile $
            do let feedCtx = postCtx `mappend` bodyField "description"
               posts <-
                 fmap (take 10) . recentFirst =<<
                 loadAllSnapshots "posts/*" "content"
               renderAtom etaFeedConfiguration feedCtx posts
     create ["rss.xml"] $
       do route idRoute
          compile $
            do let feedCtx = postCtx `mappend` bodyField "description"
               posts <-
                 fmap (take 10) . recentFirst =<<
                 loadAllSnapshots "posts/*" "content"
               renderRss etaFeedConfiguration feedCtx posts
     match "templates/*" $ compile templateCompiler

etaFeedConfiguration :: FeedConfiguration
etaFeedConfiguration =
  FeedConfiguration
  { feedTitle = "This Week in Eta"
  , feedDescription = "Provides weekly update from Eta community"
  , feedAuthorName = "Sibi"
  , feedAuthorEmail = "psibi2000+eta-weekly@gmail.com"
  , feedRoot = "http://127.0.0.1"
  }

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <> dateField "pubDate" "%FT00:00:00-05:00" <> defaultContext
