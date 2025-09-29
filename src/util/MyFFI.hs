{-# LANGUAGE ForeignFunctionInterface #-}

module MyFFI where

import Foreign.C.Types

foreign import ccall "gpio_info"
    c_gpio_info :: CInt -> CInt -> CInt

foreign import ccall "gpio_alert"
    c_gpio_alert :: CBool -> CInt

foreign import ccall "gpio_warn"
    c_gpio_warn :: CBool -> CInt

gpioAlert :: Bool -> IO Int
gpioAlert b = do
    let cb = if b then 1 else 0 :: CInt
    res <- pure $ c_gpio_alert (fromIntegral cb)
    pure (fromIntegral res)
