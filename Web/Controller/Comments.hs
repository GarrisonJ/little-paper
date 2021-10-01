module Web.Controller.Comments where

import Web.Controller.Prelude
import Web.Controller.Posts (showPost)
import Web.View.Posts.Show

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
                    redirectTo $ ShowPostAction { postId = get #postId comment }

    action DeleteCommentAction { commentId } = do
        comment <- fetch commentId
        accessDeniedUnless (get #userId comment == currentUserId)
        deleteRecord comment
        redirectTo $ ShowPostAction { postId = get #postId comment }