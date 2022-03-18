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
    <tr style={notficationOpacity notification}>
        <td>{notficationIcon notification}</td>
         <td class="profile-link">
            <img class="border rounded-circle mx-auto" src={picturePath $ get #userWhoFiredNotification notification} style="width:50px; height: 50px"/>
        </td>
        <td>{notficationTypeText notification}</td>
    </tr>
|]
    where
        postBody = fromMaybe "" $ get #postId notification |> fmap (get #body)
        usernameOfTriggerer = get #username $ get #userWhoFiredNotification notification

notficationOpacity notification = if hasPostBeenViewed notification then "opacity: 0.5;" else "opacity: 1;" :: Text

hasPostBeenViewed notification = isJust $ get #viewedAt notification

picturePath user = fromMaybe "/space.jpeg" (get #pictureUrl user)

getPostLink notfication = case get #postId notfication of
    Just post -> pathTo $ ShowPostAction $ get #id post
    Nothing -> ""

notficationIcon notification = case get #notificationType notification of
    UserLikedPost -> heartIcon
    UserCommentedOnPost -> chatIcon

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
    where
        postBody = fromMaybe "" $ get #postId notification |> fmap (get #body)
        commentBody = fromMaybe "" $ get #commentId notification |> fmap (get #body)
        usernameOfTriggerer = get #username $ get #userWhoFiredNotification notification
        notificationType = get #notificationType notification
        link = getPostLink notification

heartIcon = [hsx|
<span style="color:#ff5e57;">
    <svg xmlns="http://www.w3.org`/2000/svg" width="32" height="32" fill="currentColor" class="bi bi-heart-fill" viewBox="0 0 16 16">
        <path fill-rule="evenodd" d="M8 1.314C12.438-3.248 23.534 4.735 8 15-7.534 4.736 3.562-3.248 8 1.314z"/>
    </svg>
</span>
|]

chatIcon = [hsx|
<span>
    <svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" fill="currentColor" class="bi bi-chat-left" viewBox="0 0 16 16">
    <path d="M14 1a1 1 0 0 1 1 1v8a1 1 0 0 1-1 1H4.414A2 2 0 0 0 3 11.586l-2 2V2a1 1 0 0 1 1-1h12zM2 0a2 2 0 0 0-2 2v12.793a.5.5 0 0 0 .854.353l2.853-2.853A1 1 0 0 1 4.414 12H14a2 2 0 0 0 2-2V2a2 2 0 0 0-2-2H2z"/>
    </svg>
</span>
|]