module Config where

questions = ["1."++show i | i <- [1..10]] ++ ["2."++show i | i <- [1..7]] ++ ["3."++show i | i <- [1..15]]
answers = ["-", "A", "B", "C", "D", "E", "X"]
pageTitle = "DDA-Tracker"

dbName = "answers.db"
stylesheetName = "dda-tracker.css"

idMax :: Integer
idMax = 2^64