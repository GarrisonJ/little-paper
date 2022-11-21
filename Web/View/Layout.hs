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
        contentClasses = (if wide then "col-12" else "col-12 col-md-6") ++ (" pm-4" :: Text)

topnav:: Html
topnav = case currentUserOrNothing of
    Nothing -> [hsx|
    <nav class="navbar d-block sticky-top">
        <a class="navbar-brand" href="/">
            <img class="nav-logo" src={ assetPath "/logo.png" } />
            Daily
        </a>
    </nav>
    |]
    Just _ -> [hsx|
        <nav class="py-0 navbar navbar-expand navbar-light sticky-top nav-fill">
            <a class="navbar-brand ml-auto" href="/">
                <img class="nav-logo" src={ assetPath "/logo.png" } />
                Daily
            </a>
            <div class="nav-item ml-auto">
                <a class="text-reset" href={NotificationsAction}>
                    {bellIcon notficationCount}
                </a>
            </div>
            <div class="nav-item collapse navbar-collapse" id="navbarNavDropdown">
                <ul class="nav navbar-nav ml-auto">
                    <div class="header-spacer" style="width: 38px;"></div>
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

notficationCount :: (?context :: ControllerContext) => Int
notficationCount = count
        where
            notficationCount = fromFrozenContext :: NotficationCount
            count = case notficationCount of NotficationCount n -> n

bellIcon notficationCount = [hsx|
<div class="float-right">
    <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-bell" viewBox="0 0 16 16">
    <path d="M8 16a2 2 0 0 0 2-2H6a2 2 0 0 0 2 2zM8 1.918l-.797.161A4.002 4.002 0 0 0 4 6c0 .628-.134 2.197-.459 3.742-.16.767-.376 1.566-.663 2.258h10.244c-.287-.692-.502-1.49-.663-2.258C12.134 8.197 12 6.628 12 6a4.002 4.002 0 0 0-3.203-3.92L8 1.917zM14.22 12c.223.447.481.801.78 1H1c.299-.199.557-.553.78-1C2.68 10.2 3 6.88 3 6c0-2.42 1.72-4.44 4.005-4.901a1 1 0 1 1 1.99 0A5.002 5.002 0 0 1 13 6c0 .88.32 4.2 1.22 6z"/>
    </svg>
    {count}
</div>
|]
    where
        count = if notficationCount > 0
            then [hsx|
                <span class="position-absolute top-0 start-100 translate-middle badge rounded-pill bg-danger icon">
                    {notficationCount}
                </span>|]
            else mempty

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link href="//cdn.quilljs.com/1.3.6/quill.snow.css" rel="stylesheet">
        <link rel="stylesheet" href={assetPath "/app.css"}/>
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
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/newPost.js"}></script>
        <script src={assetPath "/app.js"}></script>
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
