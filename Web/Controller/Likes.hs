module Web.Controller.Likes where

import Web.Controller.Prelude

instance Controller LikesController where
    action CreateLikeAction = do
        ensureIsUser

        -- Create the new like record
        let newLike = newRecord @Like
                    |> fill @'["postId"]
                    |> set #userId currentUserId

        -- Find the old like record if it exists
        oldLike <- query @Like
                    |> filterWhere (#postId, get #postId newLike)
                    |> filterWhere (#userId, get #userId newLike)
                    |> fetchOneOrNothing

        case oldLike of
            -- If the old like record exists, delete it and send
            -- False because the user unliked the post
            (Just l) -> do
                deleteRecord l
                renderJson False
            -- If we didn't find the old like, then create the new like
            Nothing ->
                newLike
                    |> ifValid \case
                        Left _ -> do
                            -- Some error happened, we are still going to return False
                            -- and continue like nothing happened.
                            renderJson False
                        Right _ -> do
                            newLike
                                |> createRecord
                            renderJson True