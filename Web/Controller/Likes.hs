module Web.Controller.Likes where

import Web.Controller.Prelude
import qualified Basement.Compat.Base as GHC.Generics
import Data.Maybe (fromJust)

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
                        -- Find old notification record if it exists
                        oldNotification <- query @Notification
                                            |> filterWhere (#userWhoFiredNotification, get #userId l)
                                            |> filterWhere (#postId, Just $ get #postId l)
                                            |> fetchOneOrNothing
                        unless (isNothing oldNotification) $ do
                            deleteRecord $ fromJust oldNotification

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
                                    -- Create the notification record
                                    userToNotify <- query @User
                                                        |> filterWhereNot (#id, currentUserId)
                                                        |> innerJoin @Post (#id, #userId)
                                                        |> filterWhereJoinedTable @Post (#id, get #postId newLike)
                                                        |> fetchOneOrNothing

                                    unless (isNothing userToNotify) $ do
                                        let userToNotifyId = get #id $ fromJust userToNotify
                                        newRecord @Notification
                                            |> set #userWhoFiredNotification currentUserId
                                            |> set #postId (Just $ get #postId newLike)
                                            |> set #notificationType UserLikedPost
                                            |> set #userToNotify userToNotifyId
                                            |> createRecord
                                        pure ()

                                    newLike
                                        |> createRecord
                                    renderJson $ show Liked