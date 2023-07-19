module Graphics.Tray (
    Callback,
    MenuItem(..),
    MenuItemCb,
    MenuItemPtr,
    Tray(..),
    TrayPtr,
    defaultMenuItem,
    exitTray,
    modifyItem,
    modifyTray,
    runTrayLoop,
    separator,
) where

import Control.Exception (finally)
import Control.Monad (when)

import Foreign (alloca, poke)

import Graphics.Tray.Internal

runTrayLoop :: (TrayPtr -> Tray) -> (TrayPtr -> IO ()) -> IO ()
runTrayLoop tray act = withTray tray $
    \trayPtr -> do
        err <- c_tray_init trayPtr
        when (err < 0) $
            error "failed to create tray"

        let loop = do
                continue <- c_tray_loop True
                when (continue == 0) $ do
                    act trayPtr
                    loop

        loop `finally` c_tray_exit

withTray :: (TrayPtr -> Tray) -> (TrayPtr -> IO b) -> IO b
withTray mkTray act = alloca $
    \ptr -> do
        let trayPtr = TrayPtr ptr
            tray = mkTray trayPtr
        poke ptr tray
        act trayPtr
