{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_HADDOCK hide #-}
-- | Provides an utility functions for easy and robust workspaces' screen capturing.
module XMonad.Util.WorkspaceScreenshot.Internal
{-# WARNING "Make sure you add `initCapturing' before `xmonad' call in xmonad.hs" #-}
  ( -- * Initialization
    initCapturing
    -- * Screenshoting routines
  , captureWorkspacesWhen
  , captureWorkspacesWhenId
    -- * Defaulting
  , defaultPredicate
  , defaultHook
    -- * Screenshoting layout
  , CapturingLayout(..)
  , horizontally
  , vertically
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, foldM_, (>=>), void)
import Data.Maybe (catMaybes)
import Data.String (fromString)

import Graphics.UI.Gtk (initGUI, Rectangle(..), drawableGetSize, drawWindowGetDefaultRootWindow)
import Graphics.UI.Gtk.Gdk.Pixbuf (Colorspace(ColorspaceRgb), Pixbuf, pixbufCopyArea, pixbufGetFromDrawable, pixbufGetHeight, pixbufGetWidth, pixbufNew, pixbufSave)
import XMonad hiding (Image, Rectangle)
import qualified XMonad.StackSet as S


-- | Init gtk to enable a possibility of capturing workspaces.
initCapturing :: IO ()
initCapturing = initGUI >> return ()
{-# WARNING initCapturing "Make sure you add `initCapturing' before `xmonad' call in xmonad.hs" #-}


-- | Capture screens from workspaces satisfying given predicate.
captureWorkspacesWhen :: (WindowSpace -> X Bool) -> (FilePath -> IO ()) -> CapturingLayout -> X ()
captureWorkspacesWhen p hook = captureWorkspacesWhenId (workspaceIdToWindowSpace >=> p) hook
 where
  workspaceIdToWindowSpace i = gets $ head . filter (\w -> S.tag w == i) . S.workspaces . windowset


-- | Capture screens from workspaces which id satisfies given predicate.
captureWorkspacesWhenId :: (WorkspaceId -> X Bool) -> (FilePath -> IO ()) -> CapturingLayout -> X ()
captureWorkspacesWhenId p hook mode = do
  c <- gets $ S.currentTag . windowset
  ps <- catMaybes <$> (mapM (\t -> windows (S.view t) >> captureScreen) =<< filterM p =<< asks (workspaces . config))
  windows $ S.view c
  fp <- merge mode ps
  void . xfork $ hook fp
  return ()


-- | Default predicate. Accepts every available workspace.
defaultPredicate :: a -> X Bool
defaultPredicate = const (return True)


-- | Default hook. Does nothing.
defaultHook :: a -> IO ()
defaultHook = const (return ())


-- Capture screen with gtk pixbuf.
-- Delay is necessary to get interfaces rendered.
captureScreen :: X (Maybe Pixbuf)
captureScreen = liftIO $
  do threadDelay 100000
     rw <- drawWindowGetDefaultRootWindow
     (w, h) <- drawableGetSize rw
     pixbufGetFromDrawable rw (Rectangle 0 0 w h)


-- | Layout for resulting capture.
data CapturingLayout = CapturingLayout
  { dimensions :: [Pixbuf] -> IO (Int, Int) -- ^ Maximum height and maximum width for capture
  , fill :: [Pixbuf] -> Pixbuf -> IO () -- ^ Filling algorithm
  }


-- | Capture screens layout horizontally.
horizontally :: CapturingLayout
horizontally = CapturingLayout
  { dimensions = \xs ->
      do h <- maximum <$> mapM pixbufGetHeight xs
         w <- sum <$> mapM pixbufGetWidth xs
         return (h, w)
  , fill = \ps p -> foldM_ (addTo p) 0 ps
  }
 where
  addTo :: Pixbuf -> Int -> Pixbuf -> IO Int
  addTo p a p' =
    do w' <- pixbufGetWidth p'
       h' <- pixbufGetHeight p'
       pixbufCopyArea p' 0 0 w' h' p a 0
       return (a + w')


-- | Capture screens layout vertically.
vertically :: CapturingLayout
vertically = CapturingLayout
  { dimensions = \xs ->
     do h <- sum <$> mapM pixbufGetHeight xs
        w <- maximum <$> mapM pixbufGetWidth xs
        return (h, w)
  , fill = \ps p -> foldM_ (addTo p) 0 ps
  }
 where
  addTo :: Pixbuf -> Int -> Pixbuf -> IO Int
  addTo p a p' =
    do w' <- pixbufGetWidth p'
       h' <- pixbufGetHeight p'
       pixbufCopyArea p' 0 0 w' h' p 0 a
       return (a + h')


-- Contruct final image from the list of pixbufs.
merge :: CapturingLayout -> [Pixbuf] -> X FilePath
merge layout ps = do
  (h, w) <- liftIO $ dimensions layout ps
  p <- liftIO $ pixbufNew ColorspaceRgb False 8 w h
  liftIO $ fill layout ps p
  dir <- asks (cfgDir .directories) 
  let filepath = (dir ++ "/screenshot.png")
  liftIO $ pixbufSave p filepath (fromString "png") ([] :: [(String,String)])
  return filepath
