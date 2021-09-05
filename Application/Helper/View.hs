module Application.Helper.View where

import IHP.ViewPrelude

-- Here you can add functions which are available in all your views


placeNextToWelcomeImage right = [hsx|
    <div class="row">
        <div class="col-md-8 offset-md-2">
            <div class="row">
                <div class="col d-none d-md-block">
                    <div class="row">
                        {welcomeImage}
                    </div>
                </div>
                <div class="col">
                        <div class="d-flex justify-content-md-center align-items-center vh-100">
                            <div class="col">
                                {right}
                            </div>
                        </div>
                </div>
            </div>
        </div>
    </div>
|]
    where
        welcomeImage = [hsx|
                <div class="d-flex justify-content-md-center align-items-center vh-100">
                    <img class="img-fluid rounded-circle" src="/buddha.jpeg" alt="/ihp-welcome-icon">
                </div>
        |]
