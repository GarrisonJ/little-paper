module Web.View.Layout (defaultLayout, wideLayout, welcomePageLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import Web.Types
import Web.Routes

welcomePageLayout :: Html -> Html
welcomePageLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>{pageTitle}</title>
</head>
<body>
    <div class="container-fluid welcome-page">
        <div id="content">
            <div class="p-3 mx-auto fixed-top">
                {renderFlashMessages}
            </div>
            {inner}
        </div>
    <footer class="footer">
        <div class="container">
            <small class="text-muted float-right">
                <a class="col" title="Coming Soon">About</a>
                <a class="col" title="Coming Soon">Contact</a>
                <a class="col" title="Coming Soon">Help</a>
                <a class="col" title="Coming Soon">Developers</a>
                <a class="col" style="white-space: nowrap;" href="https://unsplash.com/photos/XXBSevpUs3Q" target="_blank">Image by Meax Prod</a>
            </small>
        </div>
    </footer>
    </div>
</body>
|]

defaultLayout :: Html -> Html
defaultLayout = layout False

wideLayout :: Html -> Html
wideLayout = layout True

layout :: Bool -> Html -> Html
layout wide inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>{pageTitle}</title>
</head>
<body>
    <div id="page-content-wrapper">
        {topnav}
        <div class="container">
            {renderFlashMessages}
            <div class="row justify-content-center">
                <div id="content" class={contentClasses}>
                    <div class="row justify-content-center">
                        <div class="w-100">
                            {inner}
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</body>
|]
    where
        contentClasses = (if wide then "col-12" else "col-12 col-md-8") ++ (" p-4" :: Text)

topnav:: Html
topnav = case currentUserOrNothing of
    Nothing -> [hsx|
    <nav class="navbar d-block sticky-top">
        <a class="navbar-brand" href="/">Daily</a>
    </nav>
    |]
    Just _ -> [hsx|
        <nav class="py-0 navbar navbar-expand navbar-light sticky-top">
        <a class="navbar-brand" href="/">Daily</a>
        <div class="collapse navbar-collapse" id="navbarNavDropdown">
            <ul class="nav navbar-nav ml-auto">
            <li class="nav-item dropdown">
                <div class="btn-group dropleft">
                <a href="#" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                    Menu
                </a>
                <div class="dropdown-menu">
                    <a class="nav-link" href={FollowedPostsAction Nothing}>Home</a>
                    <a class="nav-link" href={ShowProfileAction (get #username currentUser)}>Profile</a>
                    <a class="nav-link" href={UsersAction}>Find People</a>
                    <a class="nav-link" href={EditCurrentUserAction}>Settings</a>
                    <a class="nav-link js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>
                </div>
                </div>
            </li>
            </ul>
        </div>
        </nav>
|]

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link href="//cdn.quilljs.com/1.3.6/quill.snow.css" rel="stylesheet">
        <link rel="stylesheet" href="/app.css"/>
    |]

-- TODO: Make sure to load the correct jquery if CDN is down
scripts :: Html
scripts = [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="https://code.jquery.com/jquery-3.6.0.min.js"
                integrity="sha256-/xUj+3OJU5yExlq6GSYGSHk7tPXikynS7ogEvDej/m4="
                crossorigin="anonymous"></script>
        <!-- Start Editor libraries -->
        <script src="//cdn.quilljs.com/1.3.6/quill.min.js"></script>
        <!-- End Editor libraries -->
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script src="/vendor/turbolinks.js"></script>
        <script src="/vendor/turbolinksInstantClick.js"></script>
        <script src="/vendor/turbolinksMorphdom.js"></script>
        <script src="/helpers.js"></script>
        <script src="/ihp-auto-refresh.js"></script>
        <script src="/newPost.js"></script>
        <script src="/app.js"></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="Daily"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="Daily.computer"/>
    <link rel="apple-touch-icon" sizes="180x180" href="/favicon/apple-touch-icon.png">
    <link rel="icon" type="image/png" sizes="32x32" href="/favicon/favicon-32x32.png">
    <link rel="icon" type="image/png" sizes="16x16" href="/favicon/favicon-16x16.png">
    <link rel="manifest" href="/favicon/site.webmanifest">
    {autoRefreshMeta}
|]
