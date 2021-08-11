module Web.View.Layout (defaultLayout, welcomePageLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import Web.Types
import Web.Routes

defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>Daily</title>
</head>
<body>
    <div class="d-flex" id="wrapper">
        {sidebar}
        <div id="page-content-wrapper">
            {topnav}
            <div class="container-fluid">
                {renderFlashMessages}
                <div id="content">
                    <div class="row justify-content-center">
                        <div class="col-md-8 col-12" style="margin-top: 10px">
                            {inner}
                         </div>
                    </div>
                </div>
            </div>
        </div>

    </div>
</body>
|]

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
    </div>
</body>
|]

sidebar :: Html
sidebar = [hsx|
    <div id="sidebar-wrapper">
        <div class="sidebar-heading">
            <a href={FollowedPostsAction Nothing}>Daily</a>
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
    <nav class="navbar navbar-expand-lg navbar-light d-block d-md-none">
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
        <li class="nav-item active">
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
        <link rel="stylesheet" href="/sidebar.css"/>
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
        <script src="/sidebar.js"></script>
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
