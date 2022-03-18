module Web.Controller.Comments where

import Web.Controller.Prelude
import Web.Controller.Posts (showPost)
import Web.View.Posts.Show
import Data.Maybe (fromJust)

instance Controller CommentsController where
    beforeAction = ensureIsUser

    action CreateCommentAction = do
        let comment = newRecord @Comment
        comment
            |> fill @["postId","body"]
            |> set #userId currentUserId
            |> validateField #body (hasMaxLength 280)
            |> validateField #body nonEmpty
            |> ifValid \case
                Left comment -> showPost (get #postId comment) comment -- TODO: This is the wrong url
                Right comment -> do
                    comment <- comment |> createRecord

                    userToNotifyId <- fmap (get #id . get #userId)
                                      (fetch (get #postId comment)
                                        >>= fetchRelated #userId)
                    unless (userToNotifyId == currentUserId)
                        (newRecord @Notification
                            |> set #userWhoFiredNotification currentUserId
                            |> set #postId (Just $ get #postId comment)
                            |> set #commentId (Just $ get #id comment)
                            |> set #notificationType UserCommentedOnPost
                            |> set #userToNotify userToNotifyId
                            |> createRecord
                            >> pure ())

                    redirectTo $ ShowPostAction { postId = get #postId comment }

    action DeleteCommentAction { commentId } = do
        comment <- fetch commentId
        accessDeniedUnless (get #userId comment == currentUserId)

        notification <- query @Notification
            |> filterWhere (#userWhoFiredNotification, currentUserId)
            |> filterWhere (#commentId, Just (get #id comment))
            |> fetchOneOrNothing

        unless (isNothing notification) $ deleteRecord $ fromJust notification
        deleteRecord comment
        redirectTo $ ShowPostAction { postId = get #postId comment }