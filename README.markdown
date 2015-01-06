# XMonad-screenshot
[![Hackage](https://budueba.com/hackage/xmonad-screenshot)](https://hackage.haskell.org/package/xmonad-screenshot)
[![Build Status](https://secure.travis-ci.org/supki/xmonad-screenshot.png?branch=master)](https://travis-ci.org/supki/xmonad-screenshot)

[gtk][1]-based screen capturing utility for the [XMonad][2] window manager.
It's flexible enough to give a user options for comprehensive captured workspaces' filtering and post-capture processing.
By default it captures all existing workspaces and places resulting screenshot in `~/.xmonad/screenshot.png`

## Example screenshots

  * [Horizontal][3] layout
  * [Vertical][4] layout


## Caveats

### Installation

You may want to make sure you have [`gtk2hs-buildtools`][5] package installed and
its binaries are in `PATH` before installing `xmonad-screenshot`:

```
$ type gtk2hsC2hs
gtk2hsC2hs is /home/user/.cabal/bin/gtk2hsC2hs
```

If you do not see any encouraging output, try `cabal install gtk2hs-buildtools` and/or check
`PATH` contains `/home/user/.cabal/bin` directory

### Initialization

Due to gtk (and XMonad) constraints you need to initialize the capturing before using it.
Place call to `initCapturing` before you call `xmonad`:

```haskell
main :: IO ()
main = do
  initCapturing
  xmonad defaultConfig { ... }
```


## Usage examples
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

 [1]: https://hackage.haskell.org/package/gtk
 [2]: http://xmonad.org
 [3]: http://i.imgur.com/s9nbOaZ.png
 [4]: https://vsegda.budueba.com/img/20dacff202bb7660bae3a16250e0b3e9.jpg
 [5]: https://hackage.haskell.org/package/gtk2hs-buildtools
