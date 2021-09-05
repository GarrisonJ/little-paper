module Web.View.Users.Index where
import Web.View.Prelude

data IndexView = IndexView { users :: [User],
                             pagination :: Pagination }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="table-responsive">
            <table class="table table-hover">
                <thead>
                </thead>
                <tbody>{forEach users renderUser}</tbody>
            </table>
        </div>
        {renderPagination pagination}
    |]


renderUser :: User -> Html
renderUser user = [hsx|
    <tr data-profile-url={ShowProfileAction (get #username user)} class="profile-list-row">
        <td>
            <img class="border rounded-circle mx-auto" src={picturePath} style="width:50px; height: 50px"/>
        </td>
        <td>
            <a href={ShowProfileAction (get #username user)}>{get #username user}</a>
        </td>
        <td>
            <span>{get #bio user}</span>
        </td>
    </tr>
|]
    where
        picturePath :: Text
        picturePath = case get #pictureUrl user of
                        Nothing -> "/space.jpeg"
                        Just url -> url