module Web.View.Users.Index where
import Web.View.Prelude

data IndexView = IndexView { users :: [User] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>Users</th>
                    </tr>
                </thead>
                <tbody>{forEach users renderUser}</tbody>
            </table>
        </div>
    |]


renderUser :: User -> Html
renderUser user = [hsx|
    <tr>
        <td><a href={ShowProfileAction (get #username user)}>{get #username user}</a></td>
    </tr>
|]
