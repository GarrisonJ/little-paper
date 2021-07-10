module Web.Controller.Profiles where

import Web.Controller.Prelude
import Web.View.Users.Show

instance Controller ProfilesController where
    action ShowProfileAction { username } = do
        user <- (query @User |> filterWhere (#username, username) |> fetchOne)
                    >>= fetchRelated #posts
        render Web.View.Users.Show.ShowView { .. }
