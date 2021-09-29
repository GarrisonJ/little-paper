function initGoogleLogin() {
    gapi.load('auth2', function() {
        var element = document.getElementById('continue-with-google');
        var clientId = element.dataset.clientId;
        auth2 = gapi.auth2.init({ client_id: clientId, scope: 'profile' });
        
        auth2.attachClickHandler(element, {},
            function(googleUser) {
                var form = document.getElementById('new-session-with-google-form');
                form.querySelector('input[name="jwt"]').value = googleUser.getAuthResponse().id_token;
                form.submit();
            }, function(error) {
                console.log("Error with google oauth" , JSON.stringify(error, undefined, 2))
            });
    });
}