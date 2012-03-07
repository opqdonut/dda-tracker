import Network.CGI
import Text.XHtml
import Database.HDBC.Sqlite3 (connectSqlite3) 
import qualified Data.Map as M

import Model

nameForm c = do s <- scriptName
	        stats <- liftIO $ getStats c
                return $
                  form ! [strAttr "method" "get", strAttr "action" s]
                    << [paragraph << ("Name: " +++ textfield "name"),
    	       	        submit "" "Submit"]
                  +++
                  qtable (stat stats)
 
questions = ["ex"++show i | i <- [1..3]]
answers = ["a"++show i | i <- [1..6]]

qtable f = table << [h, concatHtml $ map row questions]
  where h = tr ! [strAttr "id" "header"] << map (td<<) ("":answers)
        row q  = tr ! [strAttr "class" q] << [td << q, concatHtml $ map (cell q) answers]
        cell q a = td ! [strAttr "id" a] << f q a

stat s q a = thespan ! [strAttr "class" "count"] << show (M.findWithDefault 0 (q,a) s)

getAnswer q = do x <- getInput q
                 return (q,x)

processAnswers conn user = do ans <- mapM getAnswer questions
                              let ans' = M.fromList [(q,a) | (q,Just a) <- ans]
			      liftIO $ putAnswers conn user ans'

queryForm conn name = 
    do 
       ans <- liftIO $ getAnswers conn name
       stats <- liftIO $ getStats conn
       let r q a | M.lookup q ans == Just a   = radio q a ! [strAttr "checked" "true"] +++ stat stats q a
       	         | otherwise                  = radio q a +++ stat stats q a
       s <- scriptName
       return $ paragraph << ("Hello " ++ name ++ "!") 
                +++ pre << show ans
                +++
       	        form ! [strAttr "method" "post", strAttr "action" (s++"/"++name)]
		   << [qtable r,
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
