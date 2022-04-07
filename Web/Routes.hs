module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types
import IHP.OAuth.Google.Types

instance AutoRoute UsersController
instance AutoRoute PasswordsController

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute PostsController
instance AutoRoute SessionsController
profilePrefix = "/user/"
instance CanRoute ProfilesController where
    parseRoute' = do
        string $ encodeUtf8 profilePrefix
        username <- remainingText
        endOfInput
        pure ShowProfileAction { username = username }

instance HasPath ProfilesController where
    pathTo ShowProfileAction { username } = profilePrefix <> username

instance AutoRoute LikesController
instance AutoRoute FollowsController
instance AutoRoute CommentsController
instance AutoRoute GoogleOAuthController
instance AutoRoute NotificationController
