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

picturePath = fromMaybe ("/space.jpeg" :: Text)

-- Icons
heartIconLarge = [hsx|
<span style="color:#ff5e57;">
    <svg xmlns="http://www.w3.org`/2000/svg" width="32" height="32" fill="currentColor" class="bi bi-heart-fill" viewBox="0 0 16 16">
        <path fill-rule="evenodd" d="M8 1.314C12.438-3.248 23.534 4.735 8 15-7.534 4.736 3.562-3.248 8 1.314z"/>
    </svg>
</span>
|]

chatIconLarge = [hsx|
<span>
    <svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" fill="currentColor" class="bi bi-chat-left" viewBox="0 0 16 16">
    <path d="M14 1a1 1 0 0 1 1 1v8a1 1 0 0 1-1 1H4.414A2 2 0 0 0 3 11.586l-2 2V2a1 1 0 0 1 1-1h12zM2 0a2 2 0 0 0-2 2v12.793a.5.5 0 0 0 .854.353l2.853-2.853A1 1 0 0 1 4.414 12H14a2 2 0 0 0 2-2V2a2 2 0 0 0-2-2H2z"/>
    </svg>
</span>
|]

personIcon = [hsx|
<svg xmlns="http://www.w3.org/2000/svg" width="32" height="32" fill="currentColor" class="bi bi-person-plus-fill" viewBox="0 0 16 16">
  <path d="M1 14s-1 0-1-1 1-4 6-4 6 3 6 4-1 1-1 1H1zm5-6a3 3 0 1 0 0-6 3 3 0 0 0 0 6z"/>
  <path fill-rule="evenodd" d="M13.5 5a.5.5 0 0 1 .5.5V7h1.5a.5.5 0 0 1 0 1H14v1.5a.5.5 0 0 1-1 0V8h-1.5a.5.5 0 0 1 0-1H13V5.5a.5.5 0 0 1 .5-.5z"/>
</svg>
|]


chatIcon = [hsx|
<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-chat-left" viewBox="0 0 16 16">
  <path d="M14 1a1 1 0 0 1 1 1v8a1 1 0 0 1-1 1H4.414A2 2 0 0 0 3 11.586l-2 2V2a1 1 0 0 1 1-1h12zM2 0a2 2 0 0 0-2 2v12.793a.5.5 0 0 0 .854.353l2.853-2.853A1 1 0 0 1 4.414 12H14a2 2 0 0 0 2-2V2a2 2 0 0 0-2-2H2z"/>
</svg>
|]

heartIcon = [hsx|
<svg xmlns="http://www.w3.org`/2000/svg" width="16" height="16" fill="currentColor" class="bi bi-heart-fill" viewBox="0 0 16 16">
    <path fill-rule="evenodd" d="M8 1.314C12.438-3.248 23.534 4.735 8 15-7.534 4.736 3.562-3.248 8 1.314z"/>
</svg>
|]

kebabHorizontal = [hsx|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 16 16" width="16" height="16" fill="currentColor">
  <path d="M8 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zM1.5 9a1.5 1.5 0 100-3 1.5 1.5 0 000 3zm13 0a1.5 1.5 0 100-3 1.5 1.5 0 000 3z"></path>
</svg>
|]