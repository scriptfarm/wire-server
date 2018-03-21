{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Journal where

import Brig.Types
import Cassandra as C
import Control.Concurrent.Async.Lifted.Safe (mapConcurrently)
import Control.Lens
import Control.Monad.Except
import Data.Id
import Data.ByteString.Conversion
import Data.Monoid ((<>))
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Proto.TeamEvents
import Galley.Types.Teams
import Galley.Types.Teams.Intra
import System.Logger (Logger)

import qualified System.Logger        as Log
import qualified Galley.Data          as Data
import qualified Galley.Intra.Journal as Journal
import qualified Galley.Aws           as Aws


runCommand :: Logger -> ClientState -> ClientState -> Bool -> Maybe (UserId, TeamId) -> IO ()
runCommand l galley brig performUpdates start = void $ C.runClient galley $ do
    page <- case start of
        Just (u,t) -> retry x5 $ paginate teamUserSelectFrom (paramsP Quorum (Identity u) 100)
        Nothing    -> retry x5 $ paginate teamUserSelect (paramsP Quorum () 100)
    scan 0 page
  where
    scan :: Int -> Page TeamUser -> C.Client ()
    scan acc page = do
        let res   = result page
        let count = acc + Prelude.length res
        mapM_ processUser res
        Log.info l . Log.msg $ Log.val ("Processed " <> toByteString' count <> " team members so far")
        when (hasMore page) $
            retry x5 (liftClient (nextPage page)) >>= scan count

    processUser :: TeamUser -> C.Client ()
    processUser (u, t) = do
        team <- retry x5 $ query1 teamSelect $ params Quorum (Identity t)
        -- Is this user part of a binding team?
        case team of
            Just (_, binding, deleted, nm, status)
                | binding == Just Binding &&
                  nm `notElem` teamsToIgnore
                                          -> handleBindingTeamUser (u, t)
                | otherwise               -> return ()
            Nothing -> liftIO $ print "No such team??"

    handleBindingTeamUser :: TeamUser -> C.Client ()
    handleBindingTeamUser (u, t) = void $ C.runClient brig $ do
        -- Check the existing user
        user <- retry x5 $ query1 userSelect $ params Quorum (Identity u)
        -- Unless the look actually returns a user (it always should!), don't
        -- update as that would cause a "broken" insert
        case user of
            Just (_, Just _,  _) -> liftIO $ print ("User: " ++ show u ++ " already has team: " ++ show t)
            -- ^ These users already have a team, don't update again
            Just (_, Nothing, _) | performUpdates -> do
                                     retry x5 $ write userUpdate $ params Quorum (t, u)
                                     liftIO $ print ("Updating user: " ++ show u ++ " with: " ++ show t)
                                 | otherwise      ->
                                     liftIO $ print ("Should update user: " ++ show u ++ " with: " ++ show t)
            -- ^ There is a user who is part of a binding team, but no team is set in the user profile
            Nothing  -> liftIO $ print ("User: " ++ show u ++ " does not exist???")

    -- These teams were/are spam related, we should permanently deleted them
    teamsToIgnore :: [Text]
    teamsToIgnore = [ "dsfewwfww" 
                    , "ВАМ 57$, ЖМИТЕ НА ССЫЛКУ » http://dengibest.ru#-ВЫВЕСТИ-БОНУС"
                    ]

-- CQL queries
teamUserSelect :: PrepQuery R () TeamUser
teamUserSelect = "SELECT user, team FROM user_team"

teamUserSelectFrom :: PrepQuery R (Identity UserId) TeamUser
teamUserSelectFrom = "SELECT user, team FROM user_team WHERE token(user) > token(?)"

teamSelect :: PrepQuery R (Identity TeamId) TeamDB
teamSelect = "SELECT team, binding, deleted, name, status FROM team WHERE team = ?"

userSelect :: PrepQuery R (Identity UserId) UserDB
userSelect = "SELECT id, team, activated FROM user WHERE id = ?"

userUpdate :: PrepQuery W (TeamId, UserId) ()
userUpdate = "UPDATE user SET team = ? WHERE id = ?"

-- Utils

type UserDB   = (UserId, Maybe TeamId, Bool)
type TeamDB   = (TeamId, Maybe TeamBinding, Bool, Text, Maybe TeamStatus)
type TeamUser = (UserId, TeamId)
type UserRow  = (UserId, Bool)
