module Web.Controller.Profiles where

import Web.Controller.Prelude
import Web.View.Users.Show

instance Controller ProfilesController where
    action ShowProfileAction { username } = do
        -- The user we are looking at
        user <- query @User
                    |> filterWhere (#isConfirmed, True)
                    |> filterWhere (#username, username)
                    |> fetchOne
                    >>= fetchRelated #posts

        -- Is the current user following this user?
        follow <- query @UserFollow
                        |> filterWhere (#followerId, currentUserId)
                        |> filterWhere (#followedId, get #id user)
                        |> fetchOneOrNothing
        let followed = not $ null follow

        -- Has the current user followed any of this users posts?
        likes <- query @Like
                    |> filterWhere (#userId, currentUserId)
                    |> filterWhereIn (#postId, ids (get #posts user))
                    |> fetch

        -- How many followers does this user have?
        followerCount :: Int <- query @UserFollow
            |> filterWhere (#followedId, get #id user)
            |> fetchCount
            -- A user always follows themselves, so we subtract one
            |> (fmap pred)

        -- How many posts has this user made?
        postCount :: Int <- query @Post
            |> filterWhere (#userId, get #id user)
            |> fetchCount

        render Web.View.Users.Show.ShowView { .. }