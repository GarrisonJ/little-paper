module Web.Controller.Likes where

import Web.Controller.Prelude
import qualified Basement.Compat.Base as GHC.Generics

data LikedResult = Liked
                | Unliked
                | FailedToProcess
                deriving (Show)

instance Controller LikesController where
    action CreateLikeAction = do
        case currentUserOrNothing of 
            Nothing -> renderJson $ show FailedToProcess
            Just _ -> do
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
                        renderJson $ show Unliked
                    -- If we didn't find the old like, then create the new like
                    Nothing ->
                        newLike
                            |> ifValid \case
                                Left _ -> do
                                    -- Some error happened, we are still going to return False
                                    -- and continue like nothing happened.
                                    renderJson $ show FailedToProcess
                                Right _ -> do
                                    newLike
                                        |> createRecord
                                    renderJson $ show Liked