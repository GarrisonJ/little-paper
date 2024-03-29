module Web.Controller.Profiles where

import Web.Controller.Prelude
import Web.View.Users.Show
import Application.Helper.PostsQuery

instance Controller ProfilesController where
    action ShowProfileAction { username } = do
        case username |> validateAll
                    [ hasMaxLength usernameMaxLength
                    , hasMinLength usernameMinLength
                    , isUsernameChars ]
                of
                    Failure _ -> redirectToPath "/"
                    FailureHtml _ -> redirectToPath "/"
                    Success -> do
                        -- The user we are looking at
                        user <- query @User
                                    |> filterWhere (#isConfirmed, True)
                                    |> filterWhere (#isSetup, True)
                                    |> filterWhere (#isBlocked, False)
                                    |> filterWhere (#username, username)
                                    |> fetchOneOrNothing
                        case user of
                            Nothing -> redirectToPath "/"
                            Just user -> do
                                posts :: [PostWithMeta] <- fetchPostsWithMetaForProfle (get #id user)

                                -- Is the current user following this user?
                                follow <- case currentUserOrNothing of
                                    Nothing -> pure Nothing -- there is no current user
                                    Just _  -> query @UserFollow
                                                    |> filterWhere (#followerId, currentUserId)
                                                    |> filterWhere (#followedId, get #id user)
                                                    |> fetchOneOrNothing
                                let followed = not $ null follow

                                -- Has the current user followed any of this users posts?
                                likes <- case currentUserOrNothing of
                                    Nothing -> pure [] -- there is no current user
                                    Just _  -> query @Like
                                            |> filterWhere (#userId, currentUserId)
                                            |> filterWhereIn (#postId, ids posts)
                                            |> fetch

                                -- How many followers does this user have?
                                followerCount :: Int <- query @UserFollow
                                    |> filterWhere (#followedId, get #id user)
                                    |> fetchCount
                                    -- A user always follows themselves, so we subtract one
                                    |> fmap pred

                                -- How many posts has this user made?
                                postCount :: Int <- query @Post
                                    |> filterWhere (#userId, get #id user)
                                    |> fetchCount

                                last30DaysPosts <- map (get #createdOnDay) <$> fetchLast30DaysPostsWithMetaForProfle user
                                last30DaysRange <- getLast30DaysRange user

                                today <- getUserDay $ get #timezone user
                                render Web.View.Users.Show.ShowView { .. }


getLast30DaysRange :: User -> IO [Day]
getLast30DaysRange user = do
    now <- getUserDay (get #timezone user)
    let thirtyDaysAgo = addDays (-29) now
    pure $ reverse $ enumFromTo thirtyDaysAgo now
