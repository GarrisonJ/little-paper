module Web.View.Posts.New where
import Web.View.Prelude

data NewView = NewView { post :: Post,
                         day :: Day,
                         dailyPost :: Maybe Post }

instance View NewView where
    html NewView { .. } = [hsx|
        <h1>{formatTime defaultTimeLocale "%A, %B %d, %Y" day}</h1>
        <div class="yosemite-window p-3">
            {renderForm post}
        </div>
    |]

renderForm post = formForWithOptions post options [hsx|
    <div class="form-group mb-3">
        <input type="text" name="bigPostTitle" value={fromMaybe "" $ get #bigPostTitle post} class={classes ["form-control", ("is-invalid", isInvalidTitle)]} placeholder="Title">
        {validationResult #bigPostTitle}
    </div>
    <div class="form-group mb-3">
        <input type="text" name="body" value={get #body post} class={classes ["form-control", ("is-invalid", isInvalidBody)]}  placeholder="Sub Title">
        {validationResult #body}
    </div>
    <div class="form-group mb-3">
        <div id="editor-container" class={classes [("is-invalid", isInvalidBigPostBody)]} style="height: 375px;">
           {preEscapedToHtml $ fromMaybe "" $ get #bigPostBody post}
        </div>
        {validationResult #bigPostBody}
    </div>
    <textarea name="bigPostBody" style="display:none" id="hiddenArea">
        {fromMaybe "" $ get #bigPostBody post}
    </textarea>
    <div class="text-right">
        <input class="btn btn-primary" type="submit" value="Save" />
    </div>
|]
    where
        isInvalidTitle = isJust (getValidationFailure #bigPostTitle post)
        isInvalidBody = isJust (getValidationFailure #body post)
        isInvalidBigPostBody = isJust (getValidationFailure #bigPostBody post)
        options :: FormContext Post -> FormContext Post
        options formContext =
            formContext
            |> set #formAction (pathTo CreateBigPostAction)
            |> set #formId "big-post-editor"