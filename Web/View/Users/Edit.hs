module Web.View.Users.Edit where
import Web.View.Prelude
import Web.View.Users.TimezoneSelectorHelper (allTimezones, TimezoneText)

newtype EditView = EditView { user :: User }

instance View EditView where
    html EditView { .. } = [hsx|
        <h1>Settings</h1>
        {renderForm user}
    |]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    <div>
        <h5>
            Profile Image
        </h5>

        <div style="max-width: 300px">
            <div class="form-group">
                <label for="user_picture_url">
                    <img id="user_picture_url_preview" src={picturePath user} style="width: 12rem; min-height: 12rem; min-width: 12rem" class="mt-2 img-thumbnail text-center text-muted" alt="Select Photo"/>
                    <input id="user_picture_url" type="file" name="pictureUrl" class="form-control form-control-file" style="display: none" data-preview="#user_picture_url_preview"/>
                    <a class="d-block text-muted text-center" href="#" onclick="document.getElementById('user_picture_url_preview').click()">Upload</a>
                </label>
            </div>
        </div>
    </div>

    {(textareaField #bio)}
    {(selectField #timezone allTimezones) { fieldLabel = "Prefered Timezone"}}
    {submitButton}
|]

picturePath :: User -> Text
picturePath user = fromMaybe "/space.jpeg" (get #pictureUrl user)