module Model where

import Database.HDBC.Sqlite3 (connectSqlite3)
import Database.HDBC
import Control.Monad
import qualified Data.Map as M

prepDB :: IConnection conn => conn -> IO ()
prepDB dbh =
    do tables <- getTables dbh
       when (not ("answers" `elem` tables)) $
           do run dbh "CREATE TABLE answers (\
	      	       \user TEXT NOT NULL,\
                       \question TEXT NOT NULL,\
                       \answer TEST NOT NULL,\
                       \PRIMARY KEY (user,question))" []
              return ()
       commit dbh

getAnswers :: IConnection conn => conn -> String -> IO (M.Map String String)
getAnswers conn name = 
    do rows <- quickQuery' conn "SELECT * FROM answers WHERE user = ?" [toSql name]
       return . M.fromList $ [(fromSql a,fromSql b) | [_,a,b] <- rows]

putAnswers :: IConnection conn => conn -> String -> M.Map String String -> IO ()
putAnswers conn name ans =
    do s <- prepare conn "insert or replace into answers values (?,?,?)"
       executeMany s [ map toSql [name, q, a] | (q,a) <- M.assocs ans ]
       commit conn
