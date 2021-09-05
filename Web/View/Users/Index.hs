module Web.View.Users.Index where
import Web.View.Prelude

data IndexView = IndexView { users :: [User],
                             follows :: [Id User],
                             pagination :: Pagination }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="table-responsive">
            <table class="table table-hover">
                <thead>
                </thead>
                <tbody>{forEach users (renderUser follows)}</tbody>
            </table>
        </div>
        {renderPagination pagination}
    |]


renderUser :: [Id User] -> User -> Html
renderUser follows user = [hsx|
    <tr data-profile-url={ShowProfileAction (get #username user)} class="profile-list-row">
        <td class="profile-link">
            <img class="border rounded-circle mx-auto" src={picturePath} style="width:50px; height: 50px"/>
        </td>
        <td class="profile-link">
            <a href={ShowProfileAction (get #username user)}>{get #username user}</a>
        </td>
        <td class="profile-link">
            <span>{get #bio user}</span>
        </td>
        <td>
            {followButton}
        </td>
    </tr>
|]
    where
        picturePath :: Text
        picturePath = case get #pictureUrl user of
                        Nothing -> "/space.jpeg"
                        Just url -> url

        followed = not $ null $ find (\uf -> uf == (get #id user)) follows

        followButton = if get #id user == get #id currentUser
                        then [hsx||]
                        else [hsx|
                            <button class={"btn follow-button " ++ followButtonTextClass}
                                    data-userid={tshow (get #id user)} 
                                    data-url={CreateFollowAction}>
                                    {followButtonText}
                            </button>
                        |]


        followButtonText :: Text
        followButtonText = if followed then "Unfollow" else "Follow"
        followButtonTextClass :: Text
        followButtonTextClass = if followed then "btn-light" else "btn-primary"