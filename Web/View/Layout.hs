module Web.View.Layout (defaultLayout, welcomePageLayout, Html) where

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

    <title>Daily</title>
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
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>Daily</title>
</head>
<body>
    <div id="page-content-wrapper">
        {topnav}
        <div class="container">
            <div class="row">
                {renderFlashMessages}
                <div class="col-3 d-none d-md-block">
                    {sidebar}
                </div>
                <div id="content" class="col-md-8 col-12">
                    <div class="row justify-content-center">
                        <div class="" style="margin-top: 10px">
                            {inner}
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
</body>
|]

sidebar :: Html
sidebar = [hsx|
    <div id="sidebar-wrapper">
        <div class="sidebar-heading">
            <h1>
                <a href={FollowedPostsAction Nothing}><img style="width: 50px; padding-top: 10px;" src="./logo.png"></a>
            </h1>
        </div>
        <div class="list-group list-group-flush">
            <a class="list-group-item list-group-item-action list-group-item-light p-3" href={FollowedPostsAction Nothing}>Home</a>
            <a class="list-group-item list-group-item-action list-group-item-light p-3" href={ShowProfileAction (get #username currentUser)}>Profile</a>
            <a class="list-group-item list-group-item-action list-group-item-light p-3" href={UsersAction}>Find People</a>
            <a class="list-group-item list-group-item-action list-group-item-light p-3" href={EditCurrentUserAction}>Settings</a>
            <a class="list-group-item list-group-item-action list-group-item-light p-3 js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>
        </div>
    </div>
|]

topnav:: Html
topnav = [hsx|
    <nav class="navbar navbar-expand-lg navbar-dark d-block d-md-none sticky-top">
    <a class="navbar-brand" href="#">Daily</a>
    <button class="navbar-toggler"
            type="button"
            data-toggle="collapse"
            data-target="#navbarSupportedContent"
            aria-controls="navbarSupportedContent"
            aria-expanded="false"
            aria-label="Toggle navigation">
        <span class="navbar-toggler-icon"></span>
    </button>
    <div class="collapse navbar-collapse" id="navbarSupportedContent">
        <ul class="navbar-nav mr-auto">
        <li class="nav-item">
            <a class="nav-link" href={FollowedPostsAction Nothing}>Home</a>
        </li>
        <li class="nav-item">
            <a class="nav-link" href={ShowProfileAction (get #username currentUser)}>Profile</a>
        </li>
        <li class="nav-item">
            <a class="nav-link" href={UsersAction}>Find People</a>
        </li>
        <li class="nav-item">
            <a class="nav-link" href={EditCurrentUserAction}>Settings</a>
        </li>
        <li class="nav-item">
            <a class="nav-link js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>
        </li>
        </ul>
    </div>
    </nav>
|]

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    |]

scripts :: Html
scripts = [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.6.0.slim.min.js"></script>
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
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="App"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
    {autoRefreshMeta}
|]
