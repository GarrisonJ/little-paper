function loadQuill() {
  var quill = new Quill("#editor-container", {
    modules: {
      toolbar: [
        [{ header: [1, 2, false] }],
        ["bold", "italic", "underline"],
        [{ 'list': 'ordered' }, { 'list': 'bullet' }],
        ["link"],
        ['clean']
      ]
    },
    placeholder: "Create a post...",
    theme: "snow" // or 'bubble'
  });
  // text-change might not be the right event hook. Works for now though.
  quill.on("text-change", function () {
    var html = quill.root.innerHTML;
    $("#hiddenArea").text(html);
  });
  return quill;
}


document.addEventListener('ihp:load', function() {
  if ($("#editor-container").length && !($("#editor-container").hasClass("editor-loaded"))){
    $("#editor-container").addClass("editor-loaded");
    loadQuill();
  }
});


