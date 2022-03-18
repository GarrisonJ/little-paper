module Web.Controller.Notification where

import Web.Controller.Prelude
import Web.View.Notification.Index
import qualified Admin.Controller.Prelude as Data.Maybe
import IHP.ViewPrelude (CanUpdate(updateRecord))


instance Controller NotificationController where
    action NotificationsAction = do
        (notificationQ, pagination) <- query @Notification
                                        |> orderByDesc #createdAt
                                        |> paginateWithOptions
                                        (defaultPaginationOptions
                                            |> set #maxItems 10)
        notifications <- notificationQ
                            |> filterWhere (#userToNotify, currentUserId)
                            |> fetch
                            >>= collectionFetchRelated #userWhoFiredNotification
                            >>= collectionFetchRelatedOrNothing #postId
                            >>= collectionFetchRelatedOrNothing #commentId

        -- Loading this page counts as viewing the notification
        -- Update the viewedAt field for each post seen
        let idsToUpdate = map (get #id) $ filter (isNothing . get #viewedAt) notifications
        now <- liftIO getCurrentTime
        mapM_ (\id -> do
            notficationToUpdate <- fetch id
            notficationToUpdate
                |> set #viewedAt (Just now)
                |> updateRecord) idsToUpdate

        render IndexView { .. }