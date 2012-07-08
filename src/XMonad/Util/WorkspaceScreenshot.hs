{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
module XMonad.Util.WorkspaceScreenshot
  ( allWorkspaces, allWorkspacesExcept, Mode(..)
  ) where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Concurrent (threadDelay)
import Control.Monad (foldM, foldM_, void)
import Data.List ((\\))
import System.Directory (createDirectory, getAppUserDataDirectory, removeDirectoryRecursive)
import System.FilePath ((</>), (<.>))
import System.Process (getProcessExitCode, runProcess)

import Graphics.GD (Image, copyRegion, imageSize, loadPngFile, newImage, savePngFile)
import XMonad hiding (Image)
import XMonad.StackSet (currentTag, view)


allWorkspaces ∷ Mode → X ()
allWorkspaces = allWorkspacesExcept []


allWorkspacesExcept ∷ [WorkspaceId] → Mode → X ()
allWorkspacesExcept blacklist mode = do
  liftIO $ do
    removeDirectoryRecursive temp_directory_path
    createDirectory temp_directory_path
  c ← gets (currentTag . windowset)
  ts ← asks ((\\ blacklist) . workspaces . config)
  max_i ← pred <$> foldM save_workspace 0 ts
  windows $ view c
  void $ xfork $ merge mode $ map (\i → temp_directory_path </> show i <.> "png") [0..max_i]
 where
  temp_directory_path = "/tmp/XMonad.screenshots"

  save_workspace i t = do
    windows $ view t
    runScrot temp_directory_path i
    return $ succ i


runScrot ∷ String → Int → X ()
runScrot p (show → name) = liftIO $ do
  threadDelay 500000
  handle ← runProcess "/usr/bin/scrot" ["--silent", "--", name <.> "png"] (Just p) Nothing Nothing Nothing Nothing
  check handle
 where
  check handle = getProcessExitCode handle >>= maybe (threadDelay 1000 >> check handle) (const $ return ())


data Mode = H | V
data XImage = HImage { image ∷ Image, height ∷ Int, width ∷ Int }
            | VImage { image ∷ Image, height ∷ Int, width ∷ Int }

loadImage ∷ Mode → FilePath → IO XImage
loadImage m fp = do
  i ← loadPngFile fp
  (w,h) ← imageSize i
  return $ case m of
    H → HImage {image = i, height = h, width = w}
    V → VImage {image = i, height = h, width = w}

max_height ∷ [XImage] → Int
max_height [] = 0
max_height xs@(HImage {}:_) = maximum $ map height xs
max_height xs@(VImage {}:_) = sum $ map height xs

max_width ∷ [XImage] → Int
max_width [] = 0
max_width xs@(HImage {}:_) = sum $ map width xs
max_width xs@(VImage {}:_) = maximum $ map width xs


merge ∷ Mode → [FilePath] → IO ()
merge mode files = do
  images ← mapM (loadImage mode) files
  new_image ← newImage $ (max_width &&& max_height) images
  foldM_ (addTo new_image) 0 images
  dir ← getAppUserDataDirectory "xmonad"
  savePngFile (dir </> "screenshot.png") new_image
 where
  addTo n s (HImage { image = i, width = w, height = h }) = copyRegion (0,0) (w, h) i (s, 0) n >> return (s + w)
  addTo n s (VImage { image = i, width = w, height = h }) = copyRegion (0,0) (w, h) i (0, s) n >> return (s + h)
