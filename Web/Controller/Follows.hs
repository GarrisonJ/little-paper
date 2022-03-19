module Web.Controller.Follows where

import Web.Controller.Prelude
import Web.Controller.Prelude (Notification'(userToNotify))
import Data.Maybe (fromJust)

data LikedResult = Followed
                | UnFollowed
                | FailedToProcess
                deriving (Show)

instance Controller FollowsController where
    action CreateFollowAction = do
        when (isNothing currentUserOrNothing) $ do
            renderJson $ show FailedToProcess

        let userId = param @(Id User) "id"

        -- Users cannot unfollow themselves
        accessDeniedUnless (userId /= currentUserId)

        follow <- query @UserFollow
            |> filterWhere (#followerId, currentUserId)
            |> filterWhere (#followedId, userId)
            |> fetchOneOrNothing

        case follow of
            Just f -> do
                deleteRecord f
                deleteNotfication currentUserId userId
                renderJson $ show UnFollowed
            Nothing -> newRecord @UserFollow
                |> set #followerId currentUserId
                |> set #followedId userId
                |> ifValid \case
                    Left _ -> renderJson $ show FailedToProcess
                    Right follow -> do
                        follow |> createRecord
                        createNotification currentUserId userId
                        renderJson $ show Followed

createNotification :: (?modelContext::ModelContext) => Id User -> Id User -> IO ()
createNotification follower followed = do
    newRecord @Notification
        |> set #userWhoFiredNotification follower
        |> set #userToNotify followed
        |> set #notificationType UserFollowed
        |> createRecord
    pure ()

deleteNotfication :: (?modelContext::ModelContext) => Id User -> Id User -> IO ()
deleteNotfication follower followed = do
    recordToDelete <- query @Notification
        |> filterWhere (#userWhoFiredNotification, follower)
        |> filterWhere (#userToNotify, followed)
        |> filterWhere (#notificationType, UserFollowed)
        |> fetchOneOrNothing

    unless (isNothing recordToDelete) do
        deleteRecord $ fromJust recordToDelete
