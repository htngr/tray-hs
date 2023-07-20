{-# LANGUAGE CPP #-}

module Graphics.Tray.Internal where

import Foreign
import Foreign.C

#ifdef mingw32_HOST_OS
#define TRAY_WINAPI 1
#elif (linux_HOST_OS)
#define TRAY_APPINDICATOR 1
#elif (darwin_HOST_OS)
#define TRAY_APPKIT 1
#endif

#include "tray.h"

data Tray = Tray
    { trayIcon :: !FilePath
    , trayTooltip :: !String
    , trayMenu :: ![MenuItem]
    }
    deriving (Eq, Show)

newtype TrayPtr = TrayPtr (Ptr Tray)

data MenuItem = MenuItem
    { menuText :: !(Maybe String)
    , menuDisabled :: !Bool
    , menuIsCheckbox :: !Bool
    , menuChecked :: !Bool
    , menuCallback :: !Callback
    , menuChildren :: ![MenuItem]
    }

data Callback
    = Cb !MenuItemCb
    | CbPtr !(FunPtr MenuItemCb)
    | Noop

instance Eq MenuItem where
    a == b =
        menuText a == menuText b
            && menuDisabled a == menuDisabled b
            && menuIsCheckbox a == menuIsCheckbox b
            && menuChecked a == menuChecked b
            && menuChildren a == menuChildren b

instance Show MenuItem where
    show (MenuItem{..}) =
        "MenuItem " <> show (menuText, menuDisabled, menuIsCheckbox, menuChecked, menuChildren)

newtype MenuItemPtr = MenuItemPtr (Ptr MenuItem)

type MenuItemCb = MenuItemPtr -> IO ()

foreign import ccall "wrapper"
    wrapCb :: MenuItemCb -> IO (FunPtr MenuItemCb)

defaultMenuItem :: MenuItem
defaultMenuItem =
    MenuItem
        { menuText = Nothing
        , menuDisabled = False
        , menuIsCheckbox = False
        , menuChecked = False
        , menuCallback = Noop
        , menuChildren = []
        }

separator :: MenuItem
separator = defaultMenuItem{menuText = Just "-"}
instance Storable Tray where
    sizeOf _ = #{size struct tray}
    alignment _ = #{alignment struct tray}
    peek ptr = do
        trayIcon <- #{peek struct tray, icon} ptr >>= peekCString
        trayTooltip <- #{peek struct tray, tooltip} ptr >>= peekCString
        trayMenu <- peekMenuItems (#{ptr struct tray, menu} ptr)
        return (Tray {..})
    poke ptr (Tray {..}) = do
        newCString trayIcon >>= #{poke struct tray, icon} ptr
        newCString trayTooltip >>= #{poke struct tray, tooltip} ptr
        pokeMenuItems (#{ptr struct tray, menu} ptr) trayMenu
      
instance Storable MenuItem where
    sizeOf _ = #{size struct tray_menu}
    alignment _ = #{alignment struct tray_menu}
    peek ptr = do
        menuText <- #{peek struct tray_menu, text} ptr >>= maybePeek peekCString 
        menuDisabled <- #{peek struct tray_menu, disabled} ptr
        menuChecked <- #{peek struct tray_menu, checked} ptr
        menuIsCheckbox <- #{peek struct tray_menu, checkbox} ptr
  
        cbPtr <- #{peek struct tray_menu, cb} ptr 
        let menuCallback =
                if cbPtr == nullFunPtr
                    then Noop
                    else CbPtr cbPtr
        menuChildren <- peekMenuItems (#{ptr struct tray_menu, submenu} ptr)
        return (MenuItem {..})
    poke ptr (MenuItem {..}) = do
        case menuText of
            Just string -> newCString string >>= #{poke struct tray_menu, text} ptr
            Nothing -> #{poke struct tray_menu, text} ptr nullPtr
        #{poke struct tray_menu, disabled} ptr menuDisabled
        #{poke struct tray_menu, checked} ptr menuChecked
        #{poke struct tray_menu, checkbox} ptr menuIsCheckbox
        case menuCallback of
            Noop -> #{poke struct tray_menu, cb} ptr nullFunPtr
            Cb cb -> wrapCb cb >>= #{poke struct tray_menu, cb} ptr
            CbPtr cbPtr -> #{poke struct tray_menu, cb} ptr cbPtr
        #{poke struct tray_menu, context} ptr nullPtr
        pokeMenuItems (#{ptr struct tray_menu, submenu} ptr) menuChildren
      
pokeMenuItems :: Ptr (Ptr MenuItem) -> [MenuItem] -> IO ()
pokeMenuItems ptr [] = poke ptr nullPtr
pokeMenuItems ptr items = newArray0 defaultMenuItem items >>= poke ptr

peekMenuItems :: Ptr (Ptr MenuItem) -> IO [MenuItem]
peekMenuItems ptr = do
    addr <- peek ptr
    if addr == nullPtr
        then return []
        else peekArray0 defaultMenuItem addr

foreign import ccall safe "tray_init"
    c_tray_init :: TrayPtr -> IO Int

foreign import ccall safe "tray_loop"
    c_tray_loop :: Bool -> IO Int

foreign import ccall safe "tray_update"
    c_tray_update :: TrayPtr -> IO ()

foreign import ccall safe "tray_exit"
    c_tray_exit :: IO ()

modifyTray :: TrayPtr -> (Tray -> Tray) -> Callback
modifyTray (TrayPtr ptr) f = Cb $
    \_ -> do
        tray0 <- peek ptr
        let tray1 = f tray0
        poke ptr tray1
        c_tray_update (TrayPtr ptr)

modifyItem :: TrayPtr -> (MenuItem -> MenuItem) -> Callback
modifyItem trayPtr f = Cb $
    \(MenuItemPtr ptr) -> do
        item0 <- peek ptr
        let item1 = f item0
        poke ptr item1
        c_tray_update trayPtr

exitTray :: Callback
exitTray = Cb (const c_tray_exit)
