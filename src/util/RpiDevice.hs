{-# LANGUAGE OverloadedStrings #-}

module RpiDevice (gpioAlert, gpioInfo, gpioWarn) where

import GPIO

gpioInfo a = withGPIO $ do
                setPinFunction Pin15 Output
                writePin Pin15 a

gpioWarn a = withGPIO $ do
                setPinFunction Pin13 Output
                writePin Pin13 a

gpioAlert a = withGPIO $ do
                setPinFunction Pin11 Output
                writePin Pin11 a