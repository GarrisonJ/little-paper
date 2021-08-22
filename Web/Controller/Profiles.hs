module Web.Controller.Profiles where

import Web.Controller.Prelude
import Web.View.Users.Show

instance Controller ProfilesController where
    action ShowProfileAction { username } = do
        user <- query @User
                    |> filterWhere (#isConfirmed, True)
                    |> filterWhere (#username, username)
                    |> fetchOne
                    >>= fetchRelated #posts

        follow <- query @UserFollow
                        |> filterWhere (#followerId, currentUserId)
                        |> filterWhere (#followedId, get #id user)
                        |> fetchOneOrNothing

        let followed = not $ null follow

        likes <- query @Like
                    |> filterWhere (#userId, currentUserId)
                    |> filterWhereIn (#postId, ids (get #posts user))
                    |> fetch

        render Web.View.Users.Show.ShowView { .. }