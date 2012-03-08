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
                  \answer TEXT,\
                  \PRIMARY KEY (user,question))" []
          return ()
     when (not ("log" `elem` tables)) $
       do run dbh "CREATE TABLE log (\
                  \user TEXT NOT NULL,\
                  \question TEXT NOT NULL,\
                  \answer TEXT NOT NULL,\
                  \timestamp INTEGER)" []
          return ()
     run dbh "CREATE TRIGGER IF NOT EXISTS log UPDATE ON answers \
             \FOR EACH ROW WHEN old.answer != new.answer BEGIN \
             \INSERT INTO log values (new.user, new.question, new.answer, strftime('%s','now')); \
             \END" []
     commit dbh

getAnswers :: IConnection conn => conn -> String -> IO (M.Map String String)
getAnswers conn name = 
    do rows <- quickQuery' conn "SELECT question, answer FROM answers WHERE user = ?" [toSql name]
       return . M.fromList $ [(fromSql a,fromSql b) | [a,b] <- rows]

putAnswers :: IConnection conn => conn -> String -> M.Map String String -> IO ()
putAnswers conn name ans =
    do i <- prepare conn "insert or ignore into answers (user,question) values (?,?)"
       executeMany i (map tail dat)
       u <- prepare conn "update answers set answer = ? where user = ? and question = ?"
       executeMany u dat
       commit conn
  where dat = [ [toSql a, toSql name, toSql q] | (q,a) <- M.assocs ans ]

getStats :: IConnection conn => conn -> IO (M.Map (String,String) Int)
getStats conn =
    do s <- quickQuery' conn "SELECT question,answer,count(*) FROM answers GROUP BY question,answer" []
       return . M.fromList $ [((fromSql a, fromSql b), fromSql c) | [a,b,c] <- s]
