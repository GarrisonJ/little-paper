module Web.View.Static.Welcome where
import Web.View.Prelude

data WelcomeView = WelcomeView

instance View WelcomeView where
    beforeRender view = do
        setLayout welcomePageLayout

    html WelcomeView = [hsx|
    <div class="row">
        <div class="col d-none d-md-block">
            <div class="row">
                <div class="d-flex justify-content-md-center align-items-center vh-100">
                    <img class="img-fluid rounded-circle" src="/buddha.jpeg" alt="/ihp-welcome-icon">
                </div>
            </div>
        </div>
        <div class="col">
            <div class="row">
                <div class="d-flex justify-content-md-center align-items-center vh-100">
                    <div class="col">
                        <div class="row p-3">
                            <h1 class="display-1">Daily</h1>
                            <p>A site where you can share something everyday.</p>
                            <p>Join Today!</p>
                        </div>
                        <div class="row p-3">
                            <a type="button" class="btn btn-primary btn-lg" href={NewSessionAction}>Log in</a><br>
                        </div>
                        <div class="row p-3">
                            <a type="button" class="btn btn-secondary btn-lg" href={NewUserAction}>Sign up</a>
                        </div>
                        <div style="bottom: 10px; position: fixed;">
                            <small class="text-muted">
                                <a class="col" title="Coming Soon">About</a>
                                <a class="col" title="Coming Soon">Contact</a>
                                <a class="col" title="Coming Soon">Help</a>
                                <a class="col" title="Coming Soon">Developers</a>
                                <a class="col" href="https://unsplash.com/photos/XXBSevpUs3Q" target="_blank">Image by Meax Prod</a>
                            </small>
                        </div>
                    </div>
                </div>
            </div>
        </div>
    </div>
|]