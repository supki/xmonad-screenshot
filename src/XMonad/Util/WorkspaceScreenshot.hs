{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module XMonad.Util.WorkspaceScreenshot
  ( allWorkspaces, allWorkspacesExcept, Mode(..)
  ) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Control.Monad (foldM_, void)
import Data.List ((\\))
import Data.Maybe (catMaybes)
import System.Directory (getAppUserDataDirectory)
import System.FilePath ((</>), (<.>))

import Graphics.UI.Gtk.Gdk.Drawable (drawableGetSize)
import Graphics.UI.Gtk.Gdk.DrawWindow (drawWindowGetDefaultRootWindow)
import Graphics.UI.Gtk.Gdk.Events (Rectangle(..))
import Graphics.UI.Gtk.Gdk.Pixbuf (Colorspace(ColorspaceRgb), Pixbuf, pixbufCopyArea, pixbufGetFromDrawable, pixbufGetHeight, pixbufGetWidth, pixbufNew, pixbufSave)
import XMonad hiding (Image)
import XMonad.StackSet (currentTag, view)


allWorkspaces ∷ Mode → X ()
allWorkspaces = allWorkspacesExcept []


allWorkspacesExcept ∷ [WorkspaceId] → Mode → X ()
allWorkspacesExcept blacklist mode = do
  c ← gets (currentTag . windowset)
  ts ← asks ((\\ blacklist) . workspaces . config)
  ps ← catMaybes <$> mapM (\t → windows (view t) >> captureScreen) ts
  windows $ view c
  void $ xfork $ merge mode ps


captureScreen ∷ X (Maybe Pixbuf)
captureScreen = liftIO $
  do threadDelay 100000
     rw ← drawWindowGetDefaultRootWindow
     (w, h) ← drawableGetSize rw
     pixbufGetFromDrawable rw (Graphics.UI.Gtk.Gdk.Events.Rectangle 0 0 w h)


data Mode = H | V


max_height ∷ Mode → [Pixbuf] → IO Int
max_height _ [] = return 0
max_height H xs = maximum <$> mapM pixbufGetHeight xs
max_height V xs = sum <$> mapM pixbufGetHeight xs


max_width ∷ Mode → [Pixbuf] → IO Int
max_width _ [] = return 0
max_width H xs = sum <$> mapM pixbufGetWidth xs
max_width V xs = maximum <$> mapM pixbufGetWidth xs


merge ∷ Mode → [Pixbuf] → IO ()
merge mode ps = do
  w ← max_width mode ps
  h ← max_height mode ps
  p ← pixbufNew ColorspaceRgb False 8 w h
  foldM_ (addTo mode p) 0 ps
  dir ← getAppUserDataDirectory "xmonad"
  pixbufSave p (dir </> "screenshot" <.> ".png") "png" []


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
