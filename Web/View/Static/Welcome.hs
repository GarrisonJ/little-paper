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
                A tiny social network where you post once a day
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
             {loginWithGoogle}
        </div>
        <div class="row p-2">
            <a type="button" class="btn btn-outline-secondary" href={NewSessionAction}>Login</a>
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
                {loginWithGoogle}
            </div>
            <div class="row p-2">
                <a type="button" class="btn btn-outline-secondary" href={NewSessionAction}>Login</a>
            </div>

        </div>
        |]

loginWithGoogle = [hsx|
    <div class="row">
        <div class="col-md-12">
            <a data-client-id="320597488038-48jb9rncvt1mcal20fp559tm8775p63j.apps.googleusercontent.com" id="continue-with-google" class="btn btn-outline-secondary" role="button" style="text-transform:none">
                <img width="20px" style="margin-bottom:3px; margin-right:5px" alt="Google sign-in" src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/53/Google_%22G%22_Logo.svg/512px-Google_%22G%22_Logo.svg.png" />
                Login with Google
            </a>
        </div>
    </div>
    <form method="POST" action={GoogleConnectCallbackAction} id="new-session-with-google-form">
        <input type="hidden" name="jwt" value=""/>
    </form>
    <script src="/google-login.js"></script>
    <script src="https://apis.google.com/js/platform.js?onload=initGoogleLogin"></script>
|]
