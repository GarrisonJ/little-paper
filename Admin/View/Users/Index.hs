module Admin.View.Users.Index where
import Admin.View.Prelude

data IndexView = IndexView { users :: [User] }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item active"><a href={UsersAdminAction}>Users</a></li>
            </ol>
        </nav>
        <h1>Index <a href={pathTo NewUserAdminAction} class="btn btn-primary ml-4">+ New</a></h1>
        <div class="table-responsive">
            <table class="table">
                <thead>
                    <tr>
                        <th>User</th>
                        <th></th>
                        <th></th>
                        <th></th>
                    </tr>
                </thead>
                <tbody>{forEach users renderUser}</tbody>
            </table>
        </div>
    |]


renderUser :: User -> Html
renderUser user = [hsx|
    <tr>
        <td>{user}</td>
        <td><a href={ShowUserAdminAction (get #id user)}>Show</a></td>
        <td><a href={EditUserAdminAction (get #id user)} class="text-muted">Edit</a></td>
        <td><a href={DeleteUserAdminAction (get #id user)} class="js-delete text-muted">Delete</a></td>
    </tr>
|]
