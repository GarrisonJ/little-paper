$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks
    $(document).ready(function() {
        $('#post_body').on('keyup', function() {
            var body = $(this).val();
            var charCount = body.length;
            var charRemaining = 280 - charCount;
            $('#char-count').text(charRemaining);
        });
    });
});

(function () {
    // Tell the browser not to handle scrolling when restoring via the history or
    // when reloading
    if ('scrollRestoration' in history) {
        history.scrollRestoration = 'manual'
    }

    var SCROLL_POSITION = 'scroll-position'
    var PAGE_INVALIDATED = 'page-invalidated'

    // Persist the scroll position on refresh
    addEventListener('beforeunload', function () {
        sessionStorage.setItem(SCROLL_POSITION, JSON.stringify(scrollData()))
    });

    // Invalidate the page when the next page is different from the current page
    // Persist scroll information across pages
    document.addEventListener('turbolinks:before-visit', function (event) {
        if (event.data.url !== location.href) {
            sessionStorage.setItem(PAGE_INVALIDATED, 'true')
        }
        sessionStorage.setItem(SCROLL_POSITION, JSON.stringify(scrollData()))
    })

    // When a page is fully loaded:
    // 1. Get the persisted scroll position
    // 2. If the locations match and the load did not originate from a page
    // invalidation,
    // 3. scroll to the persisted position if there, or to the top otherwise
    // 4. Remove the persisted information
    addEventListener('turbolinks:load', function (event) {
        var scrollPosition = JSON.parse(sessionStorage.getItem(SCROLL_POSITION))

        if (shouldScroll(scrollPosition)) {
            scrollTo(scrollPosition.scrollX, scrollPosition.scrollY)
        } else {
            scrollTo(0, 0)
        }
        sessionStorage.removeItem(PAGE_INVALIDATED)
    });

    function shouldScroll(scrollPosition) {
        return (scrollPosition
            && scrollPosition.location === location.href
            && !JSON.parse(sessionStorage.getItem(PAGE_INVALIDATED)))
    }

    function scrollData() {
        return {
            scrollX: scrollX,
            scrollY: scrollY,
            location: location.href
        }
    }
})()

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
            if(response === "FailedToProcess") {
                window.location = "/";
            } else if(response === "Liked" ) {
                $(`*[data-postid="${postid}"]`).css("color", "#ff5e57");
                $(`*[data-postid="${postid}"] > .likes-counter`).html(function(i, val) { return +val+1 });
            } else if(response === "Unliked") {
                $(`*[data-postid="${postid}"]`).css("color", "");
                $(`*[data-postid="${postid}"] > .likes-counter`).html(function(i, val) { return +val-1 });
            } else {
                console.log("There was some issue when attempting to like", response)
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
            if(response === "FailedToProcess") {
                window.location = "/";
            } else if(response === "Followed") {
                $(`.follower-count`).html(function(i, val) { return +val+1 });
                $(`*[data-userid="${userid}"].follow-button`).text('Unfollow').button("refresh");

                $(`*[data-userid="${userid}"].follow-button`).removeClass('btn-primary');
                $(`*[data-userid="${userid}"].follow-button`).addClass('btn-light');
            } else if(response === "UnFollowed") {
                $(`.follower-count`).html(function(i, val) { return +val-1 });
                $(`*[data-userid="${userid}"].follow-button`).text('Follow').button("refresh");;

                $(`*[data-userid="${userid}"].follow-button`).removeClass('btn-light');
                $(`*[data-userid="${userid}"].follow-button`).addClass('btn-primary');
            }
            else {
                console.log("There was some issue when attempting to follow", response);
            }
        }
    )
});

// Fix but with bootstrap dropdowns and turbolinks
document.addEventListener('ihp:load', function() {
    var dropdown_buttons = document.querySelectorAll('[data-toggle="dropdown"]');

    dropdown_buttons.forEach(function(element) {
      element.addEventListener('click', function(event) {
        event.preventDefault();

        toggleDropdown(this);
      });
    });
  });