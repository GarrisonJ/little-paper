module Web.View.Notification.Index where
import Web.View.Prelude
import Text.XHtml.Frameset (image, textarea)
import IHP.Welcome.Controller (WelcomeController(WelcomeAction))
import Web.Controller.Prelude (NotficationCount(NotficationCount))

data IndexView = IndexView { notifications ::[ Include "commentId"
                                              (Include "postId"
                                              (Include "userWhoFiredNotification"
                                               Notification )) ]
                           , pagination :: Pagination
                           }

instance View IndexView where
    html IndexView { .. } = [hsx|
        <div class="table-responsive">
            <h2>Notifications</h2>
            <table class="table table-sm">
                <tbody>{forEach notifications renderNotification}</tbody>
            </table>
            {renderPagination pagination}
        </div>
    |]

renderNotification :: Include "commentId"
                     (Include "postId"
                     (Include "userWhoFiredNotification"
                      Notification )) -> Html
renderNotification notification = [hsx|
    <tr class="" style={notficationOpacity notification}>
        <td class="align-middle text-center">{notficationIcon notification}</td>
         <td class="profile-link">
            <img class="border rounded-circle mx-auto" 
                 src={picturePath $ (get #pictureUrl $ get #userWhoFiredNotification notification)}
                 style="width:50px; height: 50px"/>
        </td>
        <td>{notficationTypeText notification}</td>
    </tr>
|]
    where
        postBody = fromMaybe "" $ get #postId notification |> fmap (get #body)
        usernameOfTriggerer = get #username $ get #userWhoFiredNotification notification

notficationOpacity notification = if hasPostBeenViewed notification then "opacity: 0.5;" else "opacity: 1;" :: Text

hasPostBeenViewed notification = isJust $ get #viewedAt notification


getPostLink notfication = case get #postId notfication of
    Just post -> pathTo $ ShowPostAction $ get #id post
    Nothing -> ""

notficationIcon notification = case get #notificationType notification of
    UserLikedPost -> heartIconLarge
    UserCommentedOnPost -> chatIconLarge
    UserFollowed -> personIcon

notficationTypeText :: Include "commentId"
                      (Include "postId"
                      (Include "userWhoFiredNotification"
                       Notification )) -> Html
notficationTypeText notification = case notificationType of
    UserLikedPost -> [hsx|
        <span><a href={ShowProfileAction usernameOfTriggerer}>{usernameOfTriggerer}</a> liked your <a href={link}>post</a></span>
        <a href={link}><span class="d-block text-muted">{postBody}</span></a>
    |]
    UserCommentedOnPost -> [hsx|
        <span><a href={ShowProfileAction usernameOfTriggerer}>{usernameOfTriggerer}</a> commented on your <a href={link}>post</a></span>
        <a href={link}><span class="d-block text-muted">{commentBody}</span></a>
    |]
    UserFollowed -> [hsx|
        <span><a href={ShowProfileAction usernameOfTriggerer}>{usernameOfTriggerer}</a> followed you</span>
    |]
    where
        postBody = fromMaybe "" $ get #postId notification |> fmap (get #body)
        commentBody = fromMaybe "" $ get #commentId notification |> fmap (get #body)
        usernameOfTriggerer = get #username $ get #userWhoFiredNotification notification
        notificationType = get #notificationType notification
        link = getPostLink notification