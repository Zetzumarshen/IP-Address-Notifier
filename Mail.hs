{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (threadDelay)
import Control.Exception.Base (try, SomeException)

import Data.IORef
import Data.Text.Lazy (pack, Text)
import qualified Data.Text.Internal as T
import qualified Data.ByteString.Char8 as B

import Network.HTTP
import Network.Mail.Mime (Part, plainPart)
import Network.Mail.SMTP.Types 
import Network.Mail.SMTP 

import System.Environment (getArgs)

import Text.Email.Validate (isValid)

-- ============================================
-- Get IP Address from http://ipecho.net/plain
-- ============================================

getIPAddress :: IO String
getIPAddress = do
    result <- try (simpleHTTP $ getRequest "http://ipecho.net/plain") 
    case result of
        Left (e :: SomeException) -> do
                  putStrLn "Exception something"
                  threadDelay (3 * 10^6)
                  getIPAddress
        Right h -> do
                   addr <- getResponseBody h
                   return addr

-- ============================================
-- Mail Part : sending IP Address to my email
-- ============================================

self :: String -> Address 
self addr = Address (Just "Self") (read addr :: T.Text)

packMessage :: Show a => a -> Part
packMessage xs = plainPart (pack (show xs))
        
sendMailToSelf :: String -> String -> IO()
sendMailToSelf addr msg = do 
    let ml = simpleMail (self addr) [(self addr)] [] [] "Latest IP Address" [packMessage msg]
    renderSendMailCustom "sendmail.exe" [] ml
    
-- ============================================
-- Handle Arguments: Produce a valid email address
-- ============================================
    
handleArgs :: IO String
handleArgs = do
    x <- getArgs
    if (length x) /= 1 
        then error "Usage: \"Mail.exe youremail@domain.com\""
        else do
             let y = concat x
             if isValid (B.pack y)
                then return y 
                else error "Invalid email address" 

-- ============================================
-- Main: Compare IP Address on Loop
-- ============================================
                
main :: IO()
main = do
    putStrLn "Commencing IP Address Notifier..."
    email <-  handleArgs
    initIP <- getIPAddress
    currentIP <- newIORef initIP
    mainloop currentIP email
    where
        mainloop currentIP email = do
            putStrLn "Refreshing..."
            threadDelay (3 * 1000000) 
            newIP <- getIPAddress
            x <- readIORef currentIP
            case newIP /= x of 
                 True -> do
                         writeIORef currentIP newIP
                         putStrLn "Sending mail to self..."
                         sendMailToSelf email newIP
                         mainloop currentIP email
                 False -> mainloop currentIP email