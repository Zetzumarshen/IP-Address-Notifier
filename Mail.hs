{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (threadDelay)
import Control.Exception.Base (try, SomeException)

import Data.IORef
import qualified Data.Text.Lazy as L 
import Data.Text.Lazy (pack, Text)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Char8 as B

import Network.HTTP
import Network.Mail.Mime (Part, plainPart, Mail, Address)
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
self addr = Address (Just self_text) (T.pack addr :: T.Text)
    where
    self_text :: T.Text
    self_text = "Self"        

packMessage :: Show a => a -> Part
packMessage xs = plainPart (L.pack (show xs))
        
constructMail :: String -> String -> Mail
constructMail addr msg = simpleMail (self addr) [(self addr)] [] [] "Latest IP Address" [packMessage msg]        
        
sendMailToSelf :: String -> String -> Maybe FilePath -> IO()
sendMailToSelf addr msg pth = do 
    let ml = constructMail addr msg
    case pth of
        Nothing -> renderSendMail ml
        Just v -> renderSendMailCustom v [] ml
    
-- ============================================
-- Handle Arguments: Produce a valid email address
-- ============================================


handleArgs :: IO (String, Maybe String)
handleArgs = do
    xs <- getArgs
    if length xs == 0 || length xs > 2 
       then error "Usage: \"Mail.exe \"youremail@domain.com\" \"sendmail path (optional)\"\"" 
       else do
             let x = (head xs)
             if isValid (B.pack x)
                then do
                     let ys = drop 1 xs
                     case null ys of 
                        True -> return (x, Nothing)
                        False -> return (x, Just (head ys))
                else error "Invalid email address" 
             

-- ============================================
-- Main: Compare IP Address on Loop
-- ============================================
                
main :: IO()
main = do
    (email, smpath) <-  handleArgs
    putStrLn "Commencing IP Address Notifier..."
    currentIP <- newIORef "0.0.0.0"
    mainloop currentIP email smpath
    where
        mainloop currentIP email smpath = do
            putStrLn "Refreshing..."
            threadDelay (3 * 1000000) 
            newIP <- getIPAddress
            x <- readIORef currentIP
            case newIP /= x of 
                 True -> do
                         writeIORef currentIP newIP
                         putStrLn "Sending mail to self..."
                         sendMailToSelf email newIP smpath
                         mainloop currentIP email smpath
                 False -> mainloop currentIP email smpath