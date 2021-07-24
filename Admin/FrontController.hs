module Admin.FrontController where

import IHP.RouterPrelude
import Admin.Controller.Prelude
import Admin.View.Layout (defaultLayout)
import IHP.LoginSupport.Middleware
import Admin.Controller.Sessions

-- Controller Imports
import Admin.Controller.UsersAdmin
import Admin.Controller.Static

instance FrontController AdminApplication where
    controllers = 
        [ startPage WelcomeAction
        , parseRoute @SessionsController
        -- Generator Marker
        , parseRoute @UsersAdminController
        ]

instance InitControllerContext AdminApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @Admin
