module Main where

import Graphics.Tray

tray :: TrayPtr -> Tray
tray self =
    Tray
        { trayIcon = "document-edit-symbolic"
        , trayTooltip = "Hello From Haskell"
        , trayMenu =
            [ defaultMenuItem
                { menuText = Just "Hi"
                , menuIsCheckbox = True
                , menuCallback = modifyItem self $
                    \it -> it{menuText = (<> "a") <$> menuText it}
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
