module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    beforeRender view = do
        setLayout welcomePageLayout

    html WelcomeView = placeNextToWelcomeImage
        [hsx|
        <div class="row p-3">
            <h1 class="display-1">Daily</h1>
            <p>A site where you can share something everyday.</p>
            <p>Join Today!</p>
        </div>
        <div class="row p-3">
            <a type="button" class="btn btn-primary" href={NewSessionAction}>Login</a><br>
        </div>
        <div class="row p-3">
            <a type="button" class="btn btn-secondary do-somethingrainbow" href={NewUserAction}>Signup</a>
        </div>
        |]