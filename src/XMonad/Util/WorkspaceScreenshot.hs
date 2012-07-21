{-# LANGUAGE UnicodeSyntax #-}
-- | Provides an utility functions for easy and robust workspaces' screen capturing.
module XMonad.Util.WorkspaceScreenshot
  ( -- * Screenshoting routines
    captureWorkspaces
  , captureWorkspacesVisible
  , captureWorkspacesExcept
  , captureWorkspacesWith
  , captureWorkspacesExceptWith
  , captureWorkspacesWhen
  , captureWorkspacesWhenId
    -- * Screenshoting mode
  , Mode(..)
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (filterM, foldM_, void, (>=>))
import Data.Maybe (catMaybes, isJust)
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>), (<.>))

import Graphics.UI.Gtk.Gdk.Drawable (drawableGetSize)
import Graphics.UI.Gtk.Gdk.DrawWindow (drawWindowGetDefaultRootWindow)
import Graphics.UI.Gtk.Gdk.Events (Rectangle(..))
import Graphics.UI.Gtk.Gdk.Pixbuf (Colorspace(ColorspaceRgb), Pixbuf, pixbufCopyArea, pixbufGetFromDrawable, pixbufGetHeight, pixbufGetWidth, pixbufNew, pixbufSave)
import XMonad hiding (Image)
import qualified XMonad.StackSet as S


-- | Capture screens from all workspaces with horizontal layout.
captureWorkspaces ∷ X ()
captureWorkspaces = captureWorkspacesExceptWith [] H


-- | Capture screens from all visible workspaces with horizontal layout.
captureWorkspacesVisible ∷ X ()
captureWorkspacesVisible = captureWorkspacesWhen (return . isJust . S.stack) H


-- | Capture screens from all workspaces except blacklisted with horizontal layout.
captureWorkspacesExcept ∷ [WorkspaceId] → X ()
captureWorkspacesExcept = flip captureWorkspacesExceptWith H


-- | Capture screens from all workspaces with specified layout.
captureWorkspacesWith ∷ Mode → X ()
captureWorkspacesWith = captureWorkspacesExceptWith []

-- | Capture screens from all workspaces except blacklisted with specified layout.
captureWorkspacesExceptWith ∷ [WorkspaceId] → Mode → X ()
captureWorkspacesExceptWith blacklist =
  captureWorkspacesWhenId $ return . (`notElem` blacklist)


-- | Capture screens from specific workspaces.
captureWorkspacesWhen ∷ (WindowSpace → X Bool) → Mode → X ()
captureWorkspacesWhen p = captureWorkspacesWhenId (fromId >=> p)
  where fromId i = gets $ head . filter ((== i) . S.tag) . S.workspaces . windowset


-- | Capture screens from workspaces with specific WorkspaceId.
captureWorkspacesWhenId ∷ (WorkspaceId → X Bool) → Mode → X ()
captureWorkspacesWhenId p mode = do
  c ← gets $ S.currentTag . windowset
  ts ← filterM p =<< asks (workspaces . config)
  ps ← catMaybes <$> mapM (\t → windows (S.view t) >> captureScreen) ts
  windows $ S.view c
  void $ xfork $ merge mode ps


-- Capture screen with gtk pixbuf.
-- Delay is necessary to get interfaces rendered.
captureScreen ∷ X (Maybe Pixbuf)
captureScreen = liftIO $
  do threadDelay 100000
     rw ← drawWindowGetDefaultRootWindow
     (w, h) ← drawableGetSize rw
     pixbufGetFromDrawable rw (Graphics.UI.Gtk.Gdk.Events.Rectangle 0 0 w h)


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
-- TODO: That should be parallelized.
merge ∷ Mode → [Pixbuf] → IO ()
merge mode ps = do
  w ← maxWidth mode ps
  h ← maxHeight mode ps
  p ← pixbufNew ColorspaceRgb False 8 w h
  foldM_ (addTo mode p) 0 ps
  dir ← getAppUserDataDirectory "xmonad"
  pixbufSave p (dir </> "screenshot" <.> ".png") "png" []
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
