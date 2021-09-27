module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)
import IHP.LoginSupport.Middleware
import Web.Controller.Sessions

-- Controller Imports
import Web.Controller.Comments
import Web.Controller.Likes
import Web.Controller.Follows
import Web.Controller.Profiles
import Web.Controller.Users
import Web.Controller.Passwords
import Web.Controller.Posts
import Web.Controller.Static
import IHP.OAuth.Google.Types
import Web.Controller.GoogleOAuth

instance FrontController WebApplication where
    controllers = 
        [ startPage WelcomeAction
        , parseRoute @SessionsController
        -- Generator Marker
        , parseRoute @CommentsController
        , parseRoute @LikesController
        , parseRoute @FollowsController
        , parseRoute @ProfilesController
        , parseRoute @UsersController
        , parseRoute @PasswordsController
        , parseRoute @PostsController
        , parseRoute @GoogleOAuthController

        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User