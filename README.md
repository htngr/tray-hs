# tray-hs - bindings to [dmikushin/tray](https://github.com/dmikushin/tray), a cross platform Linux/Windows/MacOS tray


## Usage

See [demo/Main.hs](./demo/Main.hs):

```haskell
import Graphics.Tray

tray :: TrayPtr -> Tray
tray self = Tray 
    { trayIcon = "document-edit-symbolic"
    , trayTooltip = "Hello From Haskell"
    , trayMenu =
       [ defaultMenuItem
           { menuText = Just "Hi"
           , menuIsCheckbox = True
           , menuCallback = modifyItem self $
               \it -> it { menuText = (<> "a") <$> menuText it}
           }
       , defaultMenuItem
           { menuText = Just "Bye"
           , menuCallback = exitTray
           }
        ]
    }

main :: IO ()
main = runTrayLoop tray $
    \_ ->
        return ()
```

Run with `nix run git+http://github.com/htngr/tray-hs?submodules=1`
