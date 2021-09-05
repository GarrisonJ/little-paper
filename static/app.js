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

// When you click a row on the profile list page, go to the profile
$(document).on( "click", ".profile-list-row .profile-link", function(event){
    let profileUrl = $(this).parent().data('profile-url');
    Turbolinks.visit(profileUrl)
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
            Turbolinks.clearCache();
            if(response == true) {
                $(`.follower-count`).html(function(i, val) { return +val+1 });
                $(`*[data-userid="${userid}"].follow-button`).text('Unfollow').button("refresh");;

                $(`*[data-userid="${userid}"].follow-button`).removeClass('btn-primary');
                $(`*[data-userid="${userid}"].follow-button`).addClass('btn-light');
            } else {
                $(`.follower-count`).html(function(i, val) { return +val-1 });
                $(`*[data-userid="${userid}"].follow-button`).text('Follow').button("refresh");;

                $(`*[data-userid="${userid}"].follow-button`).removeClass('btn-light');
                $(`*[data-userid="${userid}"].follow-button`).addClass('btn-primary');
            }
        }
    )
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