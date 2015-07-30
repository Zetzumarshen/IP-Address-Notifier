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
-- Compare IP Address
-- ============================================

notSame :: Eq a => IORef a -> IORef a -> IO Bool
notSame lastIP currentIP = do
    x <- readIORef lastIP
    y <- readIORef currentIP
    return (x /= y)

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
    
main :: IO()
main = do
    putStrLn "Commencing IP Address Notifier..."
    email <-  handleArgs
    initIP <- getIPAddress
    lastIP <- newIORef initIP
    currentIP <- newIORef initIP
    mainloop currentIP lastIP email
    where
        mainloop currentIP lastIP email = do
            putStrLn "Refreshing..."
            threadDelay (3 * 1000000) 
            newIP <- getIPAddress
            writeIORef currentIP newIP
            cond <- lastIP `notSame` currentIP
            case cond of 
                 True -> do
                         writeIORef lastIP newIP
                         putStrLn "Sending mail to self..."
                         sendMailToSelf email newIP
                         mainloop currentIP lastIP email
                 False -> mainloop currentIP lastIP email