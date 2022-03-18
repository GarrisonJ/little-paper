module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types
import IHP.LoginSupport.Types

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

newtype NotficationCount = NotficationCount Int

data PostsController
    = PostsAction
    | FollowedPostsAction { page :: !(Maybe Int) }
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | ShowPostForDayAction { username :: !Text, day :: !Text }
    | CreatePostAction
    | CreateBigPostAction
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)

-- Authentication
instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User

data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)

data UsersController
    = UsersAction
    | NewUserAction
    | CreateUserAction
    | EditCurrentUserAction
    | FinishUserSetupAction
    | UpdateUserSetupAction
    | UpdateUserAction { userId :: !(Id User) }
    | ConfirmUserEmailAction { userId :: !(Id User), confirmationKey :: !Text }
    deriving (Eq, Show, Data)

data PasswordsController
    = NewForgotPasswordAction
    | CreateForgotPasswordAction
    | NewResetPasswordAction { userId :: !(Id User), resetToken :: !Text }
    | CreateResetPasswordAction
    deriving (Eq, Show, Data)

data ProfilesController
    = ShowProfileAction { username :: !Text }
    deriving (Eq, Show, Data)

data LikesController
    = CreateLikeAction
    deriving (Eq, Show, Data)

data FollowsController
    = CreateFollowAction
    deriving (Eq, Show, Data)

data CommentsController
    = CreateCommentAction
    | DeleteCommentAction { commentId :: !(Id Comment) }
    deriving (Eq, Show, Data)

data NotificationController
    = NotificationsAction
    deriving (Eq, Show, Data)

