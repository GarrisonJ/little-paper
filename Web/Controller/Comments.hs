module Web.Controller.Comments where

import Web.Controller.Prelude
import Web.View.Posts.Show

instance Controller CommentsController where
    beforeAction = ensureIsUser

    action CreateCommentAction = do
        let comment = newRecord @Comment
        comment
            |> buildComment
            |> set #userId (currentUserId)
            |> ifValid \case
                Left comment -> redirectTo $ ShowPostAction { postId = (get #postId comment) }
                Right comment -> do
                    comment <- comment |> createRecord
                    redirectTo $ ShowPostAction { postId = (get #postId comment) }

    action DeleteCommentAction { commentId } = do
        comment <- fetch commentId
        accessDeniedUnless (get #userId comment == currentUserId)
        deleteRecord comment
        redirectTo $ ShowPostAction { postId = (get #postId comment) }

buildComment comment = comment
    |> fill @["postId","body"]
