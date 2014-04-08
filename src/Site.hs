{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.State.Class
import           Data.ByteString                             (ByteString)
import           Data.Monoid                                 (mempty)
import qualified Data.Text                                   as T
import           Heist
import qualified Heist.Interpreted                           as I
import           Snap.Core
import           Snap.Extras.JSON
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application

instance HasPostgres (Handler b App) where
    getPostgresState = with pg get

instance HasPostgres (Handler App (AuthManager App)) where
    getPostgresState = withTop pg get

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"


------------------------------------------------------------------------------
-- | Handle new user form submit
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = method GET handleForm <|> method POST handleFormSubmit
  where
    handleForm = render "new_user"
    handleFormSubmit = registerUser "login" "password" >> redirect "/"

needsAuth :: Handler App (AuthManager App) () -> Handler App App ()
needsAuth x = with auth $ requireUser auth (redirect "/") x

authUserSplice :: Monad m =>
                  AuthUser ->
                  Splices (HeistT n m Template)
authUserSplice authUser = do
  "userLogin" ## I.textSplice (userLogin authUser)
  "userLoginCount" ## I.textSplice (T.pack $ show $ userLoginCount authUser)

authUsersSplice  :: [AuthUser] -> I.Splice AppHandler
authUsersSplice = I.mapSplices (I.runChildrenWith . authUserSplice)

getFromPostgres :: Handler App (AuthManager App) ()
getFromPostgres = do
        result <- query_ "select * from snap_auth_user limit 1"
        liftIO $ print $ head (result :: [AuthUser])
        liftIO $ print $ userLogin $ head result
        renderWithSplices "auth_user_splice" (authUserSplice $ head result)

getManyFromPostgres :: Handler App (AuthManager App) ()
getManyFromPostgres = do
        results <- query_ "select * from snap_auth_user"
        liftIO $ print (results :: [AuthUser])
        renderWithSplices "auth_users_splice" ("authUsers" ## authUsersSplice results)


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("/login",    with auth handleLoginSubmit)
         , ("/logout",   with auth handleLogout)
         , ("/new_user", with auth handleNewUser)
         , ("/user", needsAuth getFromPostgres)
         , ("/users", needsAuth getManyFromPostgres)
         , ("",          serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    p <- nestSnaplet "pg" pg pgsInit
    -- NOTE: We're using initJsonFileAuthManager here because it's easy and
    -- doesn't require any kind of database server to run.  In practice,
    -- you'll probably want to change this to a more robust auth backend.
    a <- nestSnaplet "auth" auth $
           initPostgresAuth sess p
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a p

