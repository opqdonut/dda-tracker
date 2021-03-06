module Main where

import Control.Monad
import Network.CGI
import Text.XHtml.Strict
import Database.HDBC.Sqlite3 (connectSqlite3) 
import qualified Data.Map as M
import Numeric
import System.Random

import Model
import Config

dClassName d [] = []
dClassName d (c:s) | c `elem` ['a'..'z']++['A'..'Z']++['0'..'9'] = d ++ [c] ++ dClassName "" s
                   | otherwise = dClassName "-" s
className prefix suffix = prefix ++ dClassName "-" suffix

qtable f = table << [h, concatHtml $ map row questions]
  where h = tr ! [theclass "header"] << map (td<<) ("":answers)
        row q  = tr ! [theclass (className "q" q)] << [td ! [theclass "sider"] << q,
                                              concatHtml $ map (f q) answers]

stat s q a = thespan ! [theclass ("count count-"++x)] << x
 where x = show (M.findWithDefault 0 (q,a) s)
                

getAnswer q = do x <- getInput q
                 return (q,x)

processAnswers conn user = do ans <- mapM getAnswer questions
                              let ans' = M.fromList [(q,a) | (q,Just a) <- ans]
                              liftIO $ putAnswers conn user ans'

userUrl name = do s <- scriptName
                  return $ s++"/"++name
                  
goUser n = setStatus 303 "See Other" >> userUrl n >>= redirect

userHtml conn name = 
    do ans <- liftIO $ getAnswers conn name
       stats <- liftIO $ getStats conn
       let r q a
             | M.lookup q ans == Just a = td ! [theclass ((className "a" a)++" selected")]
                                          << [radio q a ! [checked], stat stats q a]
             | otherwise                = td ! [theclass (className "a" a)]
                                          << [radio q a, stat stats q a]
       u <- userUrl name
       uri <- fmap show $ requestURI
       return $ paragraph << "You should bookmark this page immediately so that you can return to update your answers!" 
                +++
                paragraph << ("The URL of this page is: " +++ tt << uri)
                +++
                form ! [method "post", action u]
                   << [qtable r,
                       paragraph << submit "" "Save"]

userPage conn name = do m <- requestMethod
                        case m of "POST" -> processAnswers conn name >> goUser name
                                  "GET" -> userHtml conn name >>= out
                        
mainHtml c = do s <- scriptName
                stats <- liftIO $ getStats c
                return $
                  form ! [method "post", action s]
                    << paragraph << submit "" "Generate new private url"
                  +++
                  qtable (\q a -> td ! [theclass (className "a" a)] << stat stats q a)

generateUser = do i <- liftIO $ randomRIO (0,idMax)
                  return $ showHex i ""

mainPage conn = do m <- requestMethod
                   case m of "POST" -> generateUser >>= goUser
                             "GET"  -> mainHtml conn >>= out 

errorPage = out $ h1 << "Error!"
                  +++
                  p << "Invalid url :("

page t b = header << [thetitle << t,
                      thelink noHtml ! [href stylesheetName,
                                        rel "stylesheet",
                                        thetype "text/css"]]
           +++ body << (h1 << t +++ b)

out :: Html -> CGI CGIResult
out = output . renderHtml . page pageTitle

cgiMain :: CGI CGIResult 
cgiMain = do c <- liftIO $ connectSqlite3 dbName
             liftIO $ prepDB c
             p <- pathInfo
             case p of ""       -> mainPage c
                       "/"      -> errorPage
                       '/':user -> userPage c user
 
main = runCGI $ handleErrors cgiMain
