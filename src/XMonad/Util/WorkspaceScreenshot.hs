{-# LANGUAGE UnicodeSyntax #-}
-- | Provides an utility functions for easy and robust workspaces' screen capturing.
module XMonad.Util.WorkspaceScreenshot
  ( -- * Screenshoting routines
    captureWorkspacesWhen
  , captureWorkspacesWhenId
    -- * Defaulting
  , defaultPredicate
  , defaultHook
    -- * Screenshoting mode
  , Mode(..)
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, foldM_, void, (>=>))
import Data.Maybe (catMaybes)
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>), (<.>))

import Graphics.UI.Gtk (Rectangle(..), drawableGetSize, drawWindowGetDefaultRootWindow)
import Graphics.UI.Gtk.Gdk.Pixbuf (Colorspace(ColorspaceRgb), Pixbuf, pixbufCopyArea, pixbufGetFromDrawable, pixbufGetHeight, pixbufGetWidth, pixbufNew, pixbufSave)
import XMonad hiding (Image, Rectangle)
import qualified XMonad.StackSet as S


-- | Capture screens from workspaces satisfying given predicate.
captureWorkspacesWhen ∷ (WindowSpace → X Bool) → (FilePath → IO ()) → Mode → X ()
captureWorkspacesWhen p hook = captureWorkspacesWhenId (workspaceIdToWindowSpace >=> p) hook
 where
  workspaceIdToWindowSpace i = gets $ head . filter ((== i) . S.tag) . S.workspaces . windowset


-- | Capture screens from workspaces which id satisfies given predicate.
captureWorkspacesWhenId ∷ (WorkspaceId → X Bool) → (FilePath → IO ()) → Mode → X ()
captureWorkspacesWhenId p hook mode = do
  c ← gets $ S.currentTag . windowset
  ps ← catMaybes <$> (mapM (\t → windows (S.view t) >> captureScreen) =<< filterM p =<< asks (workspaces . config))
  windows $ S.view c
  void $ xfork $ merge mode ps hook


-- | Default predicate. Accepts every available workspace.
defaultPredicate ∷ a → X Bool
defaultPredicate = const (return True)


-- | Default hook. Does nothing.
defaultHook ∷ a → IO ()
defaultHook = const (return ())


-- Capture screen with gtk pixbuf.
-- Delay is necessary to get interfaces rendered.
captureScreen ∷ X (Maybe Pixbuf)
captureScreen = liftIO $
  do threadDelay 100000
     rw ← drawWindowGetDefaultRootWindow
     (w, h) ← drawableGetSize rw
     pixbufGetFromDrawable rw (Rectangle 0 0 w h)


-- | Captured screens layout.
data Mode = H -- ^ Horizontal.
          | V -- ^ Vertical.


-- Maximum height needed to construct final image.
-- If one wants horizontal layout that's just height of the tallest pixbuf in the list.
-- If one wants vertical layout that's sum of heights of pixbufs in the list.
maxHeight ∷ Mode → [Pixbuf] → IO Int
maxHeight _ [] = return 0
maxHeight H xs = maximum <$> mapM pixbufGetHeight xs
maxHeight V xs = sum <$> mapM pixbufGetHeight xs


-- Maximum width needed to construct final image.
-- If one wants horizontal layout that's sum of widths of pixbufs in the list.
-- If one wants vertical layout that's just width of the fattest pixbuf in the list.
maxWidth ∷ Mode → [Pixbuf] → IO Int
maxWidth _ [] = return 0
maxWidth H xs = sum <$> mapM pixbufGetWidth xs
maxWidth V xs = maximum <$> mapM pixbufGetWidth xs


-- Contruct final image from the list of pixbufs.
merge ∷ Mode → [Pixbuf] → (FilePath → IO ()) → IO ()
merge mode ps hook = do
  w ← maxWidth mode ps
  h ← maxHeight mode ps
  p ← pixbufNew ColorspaceRgb False 8 w h
  foldM_ (addTo mode p) 0 ps
  dir ← getAppUserDataDirectory "xmonad"
  let filepath = (dir </> "screenshot" <.> ".png")
  pixbufSave p filepath "png" []
  hook filepath
 where
  addTo ∷ Mode → Pixbuf → Int → Pixbuf → IO Int
  addTo H p a p' =
    do w' ← pixbufGetWidth p'
       h' ← pixbufGetHeight p'
       pixbufCopyArea p' 0 0 w' h' p a 0
       return (a + w')
  addTo V p a p' =
    do w' ← pixbufGetWidth p'
       h' ← pixbufGetHeight p'
       pixbufCopyArea p' 0 0 w' h' p 0 a
       return (a + h')
