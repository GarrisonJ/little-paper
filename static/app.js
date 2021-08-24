$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks


});

// The post like button
$(document).on( "click", ".like-button", function(event){
    let postid = $(this).data('postid');
    let url = $(this).data('url');

    $.ajax({
        url: url,
        type: 'post',
        data: { postId: postid }
    })
    .done(
        function(response) {
            if(response == true) {
                $(`*[data-postid="${postid}"]`).css("color", "#ff5e57");
            } else {
                $(`*[data-postid="${postid}"]`).css("color", "");
            }
        }
    )
});