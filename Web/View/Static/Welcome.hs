module Web.View.Static.Welcome where
import Web.View.Prelude
import Application.Script.Prelude (Controller)

data WelcomeView = WelcomeView { lockUserCreation :: Bool }

instance View WelcomeView where
    beforeRender view = do
        setLayout welcomePageLayout

    html WelcomeView { .. } = placeNextToWelcomeImage
        [hsx|
        <div class="row p-3 d-flex">
            <h1 class="display-1">
                <img class="welcome-logo" src={ assetPath "/logo.png" } />
                Daily
            </h1>
            <p class="w-100">
                A social network where you can post once a day
            </p>
        </div>
        {renderLoginIfNotFull lockUserCreation}
        |]

renderLoginIfNotFull lockUserCreation = if lockUserCreation
    then renderWeAreFull
    else renderLogin

renderLogin = [hsx|
        <div class="row p-2">
            <a type="button" class="btn btn-primary" href={NewUserAction}>Signup</a><br>
        </div>
        <div class="row p-2">
            <a type="button" class="btn btn-outline-secondary" href={NewSessionAction}>Login</a>
        </div>
        <div class="row p-2">
             {loginWithGoogle}
        </div>
        |]

renderWeAreFull = [hsx|
        <div>
            <div class="p-2 yosemite-window">
                <h2>We are full</h2>
                We can't handle new users right now. <br>
                Check back later, and hopefully we'll have more space.
            </div>
            <hr>
            <div class="row p-2">
                <a type="button" class="btn btn-outline-secondary" href={NewSessionAction}>Login</a>
            </div>
            <div class="row p-2">
                {loginWithGoogle}
            </div>
        </div>
        |]

loginWithGoogle = [hsx|
    <div id="g_id_onload"
         data-client_id={googleClientId}
         data-context="signin"
         data-ux_mode="popup"
         data-callback="onGoogleLogin"
         data-auto_prompt="false"
         >
    </div>

    <div class="g_id_signin"
         data-type="standard"
         data-shape="rectangular"
         data-theme="outline"
         data-text="continue_with"
         data-size="large"
         data-logo_alignment="left"
         data-width="304">
    </div>
    <form method="POST" action={GoogleConnectCallbackAction} id="new-session-with-google-form">
        <input type="hidden" name="jwt" value=""/>
    </form>
    <script src="/google-login.js?v2"></script>
    <script src="https://accounts.google.com/gsi/client"></script>
|]
    where
        googleClientId :: Text = "320597488038-48jb9rncvt1mcal20fp559tm8775p63j"