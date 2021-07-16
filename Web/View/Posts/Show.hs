module Web.View.Posts.Show where
import Web.View.Prelude

data ShowView = ShowView { post :: Include "userId" Post }

instance View ShowView where
    html ShowView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PostsAction}>Posts</a></li>
                <li class="breadcrumb-item active">Show Post</li>
            </ol>
        </nav>
        <h2>{get #createdOn post}</h2>
        <p>{get #body post}</p>
        <p>{post |> get #userId |> get #username}</p>
    |]
