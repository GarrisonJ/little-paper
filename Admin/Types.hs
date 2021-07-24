module Admin.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import IHP.LoginSupport.Types

data AdminApplication = AdminApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)


data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)


instance HasNewSessionUrl Admin where
    newSessionUrl _ = "/Admin/NewSession"

type instance CurrentAdminRecord = Admin

data UsersAdminController
    = UsersAdminAction
    | NewUserAdminAction
    | ShowUserAdminAction { userId :: !(Id User) }
    | CreateUserAdminAction
    | EditUserAdminAction { userId :: !(Id User) }
    | UpdateUserAdminAction { userId :: !(Id User) }
    | DeleteUserAdminAction { userId :: !(Id User) }
    deriving (Eq, Show, Data)
