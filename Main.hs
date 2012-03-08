module Main where

import Control.Monad
import Network.CGI
import Text.XHtml.Strict
import Database.HDBC.Sqlite3 (connectSqlite3) 
import qualified Data.Map as M

import Model

questions = ["ex"++show i | i <- [1..3]]
answers = ["a"++show i | i <- [1..6]]

qtable f = table << [h, concatHtml $ map row questions]
  where h = tr ! [theclass "header"] << map (td<<) ("":answers)
        row q  = tr ! [theclass q] << [td ! [theclass "sider"] << q,
                                              concatHtml $ map (f q) answers]

stat s q a = thespan ! [theclass "count"] << show (M.findWithDefault 0 (q,a) s)

getAnswer q = do x <- getInput q
                 return (q,x)

processAnswers conn user = do ans <- mapM getAnswer questions
                              let ans' = M.fromList [(q,a) | (q,Just a) <- ans]
                              liftIO $ putAnswers conn user ans'

userUrl name = do s <- scriptName
                  return $ s++"/"++name

userHtml conn name = 
    do ans <- liftIO $ getAnswers conn name
       stats <- liftIO $ getStats conn
       let r q a
             | M.lookup q ans == Just a = td ! [theclass (a++" selected")]
                                          << [radio q a ! [checked], stat stats q a]
             | otherwise                = td ! [theclass a]
                                          << [radio q a, stat stats q a]
       u <- userUrl name
       return $ paragraph << ("Hello " ++ name ++ "!") 
                +++
                form ! [method "post", action u]
                   << [qtable r,
                       submit "" "Save"]

userPage conn name = do m <- requestMethod
                        when (m=="POST") $ processAnswers conn name
                        userHtml conn name >>= out
                        
mainHtml c = do s <- scriptName
                stats <- liftIO $ getStats c
                return $
                  form ! [method "get", action s]
                    << [paragraph << ("Name: " +++ textfield "name"),
                        submit "" "Login"]
                  +++
                  qtable (\q a -> td ! [theclass a] << stat stats q a)

mainPage conn = do mn <- getInput "name"
                   case mn of Nothing -> mainHtml conn >>= out
                              Just n  -> setStatus 301 "Redirect" >> userUrl n >>= redirect

page t b = header << [thetitle << t,
                      thelink noHtml ! [href "dda-tracker.css",
                                        rel "stylesheet",
                                        thetype "text/css"]]
           +++ body << (h1 << t +++ b)

out :: Html -> CGI CGIResult
out = output . renderHtml . page "DDA-Tracker"

cgiMain :: CGI CGIResult 
cgiMain = do c <- liftIO $ connectSqlite3 "answers.db"
             liftIO $ prepDB c
             p <- pathInfo
             case p of ""       -> mainPage c
                       '/':user -> userPage c user
 
main = runCGI $ handleErrors cgiMain
