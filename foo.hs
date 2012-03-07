import Network.CGI
import Text.XHtml
import Database.HDBC.Sqlite3 (connectSqlite3) 
import qualified Data.Map as M

import Model

nameForm c = do s <- scriptName
                return $ form ! [strAttr "method" "get", strAttr "action" s]
                  << [paragraph << ("Name: " +++ textfield "name"),
    	       	      submit "" "Submit"]
 
questions = ["ex"++show i | i <- [1..3]]
answers = ["a"++show i | i <- [1..6]]

getAnswer q = do x <- getInput q
                 return (q,x)

processAnswers conn user = do ans <- mapM getAnswer questions
                              let ans' = M.fromList [(q,a) | (q,Just a) <- ans]
			      liftIO $ putAnswers conn user ans'

queryForm conn name = 
    do 
       ans <- liftIO $ getAnswers conn name
       let r q a | M.lookup q ans == Just a   = radio q a ! [strAttr "checked" "true"]
       	         | otherwise                  = radio q a
       s <- scriptName
       return $ paragraph << ("Hello " ++ name ++ "!") 
                +++ pre << show ans
                +++
       	        form ! [strAttr "method" "post", strAttr "action" (s++"/"++name)]
		   << [ulist << [li << (q +++ [r q a | a<-answers]) | q<-questions],
	       	       submit "" "Save"]

userPage conn name = do m <- requestMethod
                        case m of
	      	          "GET" -> return ()
			  "POST" -> processAnswers conn name
                        queryForm conn name >>= out

mainPage conn = do mn <- getInput "name"
                   s <- scriptName
                   case mn of Nothing -> nameForm conn >>= out
                              Just n  -> setStatus 301 "Redirect" >> redirect (s++"/"++n)

page t b = header << thetitle << t +++ body << b

out :: Html -> CGI CGIResult
out = output . renderHtml . page "Questionnaire"

cgiMain :: CGI CGIResult 
cgiMain = do c <- liftIO $ connectSqlite3 "answers.db"
	     liftIO $ prepDB c
             p <- pathInfo
	     case p of ""       -> mainPage c
             	       '/':user -> userPage c user
 
main = runCGI $ handleErrors cgiMain
