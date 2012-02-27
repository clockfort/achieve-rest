-- For both scotty and MYSql-Simple
{-# LANGUAGE OverloadedStrings #-}

-- For Scotty
import Web.Scotty
import Data.Monoid (mconcat)

-- MySQL
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL
import Control.Monad.IO.Class
import qualified Control.Exception as E

main = scotty 3000 $ do
	get "/v1/users/:user" $ do
		user <- param "user"
		json  user
	get "/v1/users/:user/score" $ do
		user <- param "user"
		score <- liftIO $ (fetchTotalScore user)
		json score
	get "/v1/apps/:app/description" $ do
		app <- param "app"
		appDesc <- liftIO $ fetchAppDesc app
		json appDesc
	
cshAchievementsDB = defaultMySQLConnectInfo { mysqlHost = "db.csh.rit.edu", mysqlUser= "achievements_api", mysqlPassword = "", mysqlDatabase = "achievements"}

fetchTotalScore username =
        do
                conn <- connectMySQL cshAchievementsDB
                rows <- quickQuery' conn "SELECT SUM( t1.score ) FROM achievements AS t1 INNER JOIN achievement_progress AS t2 ON t2.achievement_id = t1.id WHERE t1.progress_max = t2.progress AND t2.user_id = ( SELECT id FROM users WHERE username = ? ) " [toSql username]
                disconnect conn
		return (fromSql (rows !! 0 !! 0) :: Integer)

fetchUserID username =
	do
		conn <- connectMySQL cshAchievementsDB
		rows <- quickQuery' conn "SELECT id from users where username = ?" [toSql username]
		disconnect conn
		return (fromSql (rows !! 0 !! 0) :: Integer)

fetchAppDesc appName =
	do
		conn <- connectMySQL cshAchievementsDB
		rows <- quickQuery' conn "SELECT description from apps where name = ?" [toSql appName]
		disconnect conn
		return (fromSql (rows !! 0 !! 0) :: String)
