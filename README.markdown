#XMonad-screenshot

Simple [gtk][1]-based screen capturing utility for [XMonad][2] window manager.  
It's flexible enough to give a user options for comprehensive captured workspaces' filtering and post-capture processing.  
By default it captures all existing workspaces and places resulting screenshot in `~/.xmonad/screenshot.png`

##Screenshots examples

  * [Horizontal][3] layout
  * [Vertical][4] layout

##Usage examples
The most simple usage example:

```haskell
import import XMonad.Util.WorkspaceScreenshot

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ...
  , ((modm .|. shiftMask, xK_u), captureWorkspacesWhen defaultPredicate defaultHook H)
  , ...
  ]
```

You can filter some blacklisted workspaces from capturing using predicates:

```haskell
import import XMonad.Util.WorkspaceScreenshot

predicate x = return $ x `notElem` ["blacklistedWorkspace1", "blacklistedWorkspace2"]

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ...
  , ((modm .|. shiftMask, xK_u), captureWorkspacesWhen predicate defaultHook H)
  , ...
  ]
```

You can move screenshot file somewhere using post-processing hook:

```haskell
import Control.Monad.Trans
import System.FilePath
import System.Directory
import XMonad.Util.WorkspaceScreenshot

hook filepath = liftIO $
  do hd <- getHomeDirectory
	 renameFile filepath (hd </> "Pictures" </> filepath)

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
  [ ...
  , ((modm .|. shiftMask, xK_u), captureWorkspacesWhen defaultPredicate hook H)
  , ...
  ]
```

 [1]: http://hackage.haskell.org/package/gtk
 [2]: http://xmonad.org
 [3]: http://vsegda.budueba.com/img/03a4979e2aaddbac418c6a172f9a8479.jpg
 [4]: http://vsegda.budueba.com/img/20dacff202bb7660bae3a16250e0b3e9.jpg
