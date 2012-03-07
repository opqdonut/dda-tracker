import Network.CGI
import Text.XHtml
import Database.HDBC.Sqlite3 (connectSqlite3) 
import qualified Data.Map as M

import Model

nameForm = return $ form << [paragraph << ("My name is " +++ textfield "name"),
    	                     submit "" "Submit"]
 
questions = ["ex"++show i | i <- [1..3]]
answers = ["a"++show i | i <- [1..6]]

getAnswer q = do x <- getInput q
                 return (q,x)

processAnswers conn user = do ans <- mapM getAnswer questions
                              let ans' = M.fromList [(q,a) | (q,Just a) <- ans]
			      liftIO $ putAnswers conn user ans'

queryForm conn name = 
    do processAnswers conn name
       ans <- liftIO $ getAnswers conn name
       let r q a | M.lookup q ans == Just a   = radio q a ! [strAttr "checked" "true"]
       	         | otherwise                  = radio q a
       return $ paragraph << ("Hello " ++ name ++ "!") 
                +++ pre << show ans
                +++
       	        form << [textfield "name" ! [strAttr "hidden" "true", strAttr "value" name],
               	         ulist << [li << (q +++ [r q a | a<-answers]) | q<-questions],
	       	         submit "" "Save"]

page t b = header << thetitle << t +++ body << b

cgiMain :: CGI CGIResult 
cgiMain = do c <- liftIO $ connectSqlite3 "answers.db"
	     liftIO $ prepDB c
             mn <- getInput "name"
             x <- maybe nameForm (queryForm c) mn
             output . renderHtml $ page "Questionnaire" x
 
main = runCGI $ handleErrors cgiMain
