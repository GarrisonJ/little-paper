module Web.Controller.Follows where

import Web.Controller.Prelude

data LikedResult = Followed
                | UnFollowed
                | FailedToProcess
                deriving (Show)

instance Controller FollowsController where
    action CreateFollowAction = do
        case currentUserOrNothing of 
            Nothing -> renderJson $ show FailedToProcess
            Just _ -> do
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
                        renderJson $ show UnFollowed
                    Nothing -> newRecord @UserFollow
                        |> set #followerId currentUserId
                        |> set #followedId userId
                        |> ifValid \case
                            Left _ -> renderJson $ show FailedToProcess
                            Right follow -> do
                                follow |> createRecord
                                renderJson $ show Followed