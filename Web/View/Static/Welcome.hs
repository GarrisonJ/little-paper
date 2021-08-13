module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    beforeRender view = do
        setLayout welcomePageLayout

    html WelcomeView = placeNextToWelcomeImage
        [hsx|
        <div class="row p-3">
            <h1 class="display-1 d-none d-md-block float-left position-absolute" style="top:-160px; left: -190px; font-size: 13em;">Daily</h1>
            <h1 class="display-1 d-md-none d-block">Daily</h1>
            <p class="">A site where you can share <br/> something everyday.<br/>
             Join Now! 🎉</p>
        </div>
        <div class="row p-3">
            <a type="button" class="btn btn-primary" href={NewUserAction}>Signup</a><br>
        </div>
        <div class="row p-3">
            <a type="button" class="btn btn-outline-secondary" href={NewSessionAction}>Login</a>
        </div>
        |]