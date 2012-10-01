#XMonad-screenshot

Simple [gtk][1]-based screen capturing utility for [XMonad][2] window manager.  
It's flexible enough to give a user options for comprehensive captured workspaces' filtering and post-capture processing.  
By default it captures all existing workspaces and places resulting screenshot in `~/.xmonad/screenshot.png`

**You probably do not want to use me: used improperly you will get XMonad dead!**

##Screenshots examples

  * [Horizontal][3] layout
  * [Vertical][4] layout


##Caution
You need to initialize capturing before using (this is due to gtk contraints).  
Place call to `initCapturing` before you call `xmonad`:

```haskell
main :: IO ()
main = do
  initCapturing
  xmonad defaultConfig { ... }
```


##Usage examples
The most simple usage example:

```haskell
import XMonad.Util.WorkspaceScreenshot

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ...
  , ((modm .|. shiftMask, xK_u), captureWorkspacesWhen defaultPredicate defaultHook horizontally)
  , ...
  ]
```

You can filter some blacklisted workspaces from capturing using predicates:

```haskell
import XMonad.Util.WorkspaceScreenshot

predicate x = return $ x `notElem` ["blacklistedWorkspace1", "blacklistedWorkspace2"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ...
  , ((modm .|. shiftMask, xK_u), captureWorkspacesWhen predicate defaultHook horizontally)
  , ...
  ]
```

You can move screenshot file somewhere using post-processing hook:

```haskell
import Control.Monad.Trans
import System.FilePath
import System.Directory
import XMonad.Util.WorkspaceScreenshot

hook filepath =
  do hd <- getHomeDirectory
	 renameFile filepath (hd </> "Pictures" </> filepath)

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ...
  , ((modm .|. shiftMask, xK_u), captureWorkspacesWhen defaultPredicate hook horizontally)
  , ...
  ]
```


 [1]: http://hackage.haskell.org/package/gtk
 [2]: http://xmonad.org
 [3]: http://vsegda.budueba.com/img/03a4979e2aaddbac418c6a172f9a8479.jpg
 [4]: http://vsegda.budueba.com/img/20dacff202bb7660bae3a16250e0b3e9.jpg
