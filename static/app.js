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
                $(`*[data-postid="${postid}"] > .likes-counter`).html(function(i, val) { return +val+1 });
            } else {
                $(`*[data-postid="${postid}"]`).css("color", "");
                $(`*[data-postid="${postid}"] > .likes-counter`).html(function(i, val) { return +val-1 });
            }
        }
    )
});

// The follow button
$(document).on( "click", ".follow-button", function(event){
    let userid = $(this).data('userid');
    let url = $(this).data('url');

    $.ajax({
        url: url,
        type: 'post',
        data: { id: userid }
    })
    .done(
        function(response) {
            if(response == true) {
                $(`.follower-count`).html(function(i, val) { return +val+1 });
                $(`*[data-userid="${userid}"].follow-button`).text('Unfollow').button("refresh");;
            } else {
                $(`.follower-count`).html(function(i, val) { return +val-1 });
                $(`*[data-userid="${userid}"].follow-button`).text('Follow').button("refresh");;
            }
        }
    )
});

// When you click a row on the pofile list page, go to the profile
$(document).on( "click", ".profile-list-row", function(event){
    let profileUrl = $(this).data('profile-url');
    Turbolinks.visit(profileUrl)
});

// Always scroll to top when you click a link
document.addEventListener("turbolinks:load", function() {
    window.scrollTo(0,0)
});

// Fix but with bootstrap dropdowns and turbolinks
document.addEventListener('turbolinks:load', function() {
    var dropdown_buttons = document.querySelectorAll('[data-toggle="dropdown"]');

    dropdown_buttons.forEach(function(element) {
      element.addEventListener('click', function(event) {
        event.preventDefault();

        toggleDropdown(this);
      });
    });
  });