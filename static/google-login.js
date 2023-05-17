function onGoogleLogin(response) {
    var form = document.getElementById('new-session-with-google-form');
    form.querySelector('input[name="jwt"]').value = response.credential;
    form.submit();
}