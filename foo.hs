import Network.CGI
import Text.XHtml
 
nameForm = form << [paragraph << ("My name is " +++ textfield "name"),
    	            submit "" "Submit"]
 
questions = ["q"++show i | i <- [1..3]]
answers = ["a"++show i | i <- [1..6]]

queryForm name = form << [textfield "name" ! [strAttr "hidden" "true", strAttr "value" name],
                          ulist << [li << (q +++ [radio q a | a<-answers]) | q<-questions],
	       	 	  submit "" "Submit"]

greet n = paragraph << ("Hello " ++ n ++ "!") +++ queryForm n
 
page t b = header << thetitle << t +++ body << b
 
cgiMain = do mn <- getInput "name"
             let x = maybe nameForm greet mn
             output . renderHtml $ page "Input example" x
 
main = runCGI $ handleErrors cgiMain
