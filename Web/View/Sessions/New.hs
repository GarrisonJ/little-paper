module Web.View.Sessions.New where
import Web.View.Prelude
import IHP.AuthSupport.View.Sessions.New

instance View (NewView User) where
    beforeRender view = do
        setLayout welcomePageLayout

    html NewView { .. } = placeNextToWelcomeImage
        [hsx|
        <div class="h-100" id="sessions-new">
            <div class="d-flex justify-content-md-center align-items-center vh-100">
                <div class="w-100">
                    <div style="max-width: 400px" class="mx-auto mb-5">
                        <h1><a href="/">Daily</a></h1>
                        <h5>Please login</h5>
                        {renderForm user}
                    </div>
                </div>
            </div>
        </div>
        |]

renderForm :: User -> Html
renderForm user = [hsx|
    <form method="POST" action={CreateSessionAction}>
        <div class="form-group">
            <input name="email" value={get #email user} type="email" class="form-control" placeholder="E-Mail" required="required" autofocus="autofocus" />
        </div>
        <div class="form-group">
            <input name="password" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary btn-block">Login</button>
    </form>
|]