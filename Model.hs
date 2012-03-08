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
                  \answer TEXT NOT NULL,\
                  \PRIMARY KEY (user,question))" []
          return ()
     when (not ("log" `elem` tables)) $
       do run dbh "CREATE TABLE log (\
                  \user TEXT NOT NULL,\
                  \question TEXT NOT NULL,\
                  \answer TEXT NOT NULL,\
                  \timestamp INTEGER)" []
          return ()
     run dbh "CREATE TRIGGER IF NOT EXISTS logu UPDATE ON answers \
             \FOR EACH ROW WHEN old.answer != new.answer BEGIN \
             \INSERT INTO log values (new.user, new.question, new.answer, strftime('%s','now')); \
             \END" []
     run dbh "CREATE TRIGGER IF NOT EXISTS logi INSERT ON answers \
             \FOR EACH ROW BEGIN \
             \INSERT INTO log values (new.user, new.question, new.answer, strftime('%s','now')); \
             \END" []
     commit dbh

getAnswers :: IConnection conn => conn -> String -> IO (M.Map String String)
getAnswers conn name = 
    do rows <- quickQuery' conn "SELECT question, answer FROM answers WHERE user = ?" [toSql name]
       return . M.fromList $ [(fromSql a,fromSql b) | [a,b] <- rows]

putAnswers :: IConnection conn => conn -> String -> M.Map String String -> IO ()
putAnswers conn name ans =
    do s <- prepare conn "insert or replace into answers values (?,?,?)"
       let dat = [ map toSql [name, q, a] | (q,a) <- M.assocs ans ]
       executeMany s dat
       commit conn

getStats :: IConnection conn => conn -> IO (M.Map (String,String) Int)
getStats conn =
    do s <- quickQuery' conn "SELECT question,answer,count(*) FROM answers GROUP BY question,answer" []
       return . M.fromList $ [((fromSql a, fromSql b), fromSql c) | [a,b,c] <- s]
