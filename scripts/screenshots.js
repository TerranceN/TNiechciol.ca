$(function() {
  var overlay = $(".overlay");
  var popupContainer = $(".popup_container");
  var body = $("body");
  var scrollAmount = 0;

  var closeFcn = function () {
    overlay.removeClass("visible");
    popupContainer.removeClass("visible");
    body.css('top', 0);
    body.removeClass("popup");
    body.scrollTop(scrollAmount);
  }
  $(".close", popupContainer).click(closeFcn);

  $(".screenshot").click(function() {
    var newSrc = $("img", $(this))[0].src.replace(/^(.*)_small([0-9a-f]{40,40}).jpg$/, function(match, p1, p2) {
      return p1 + "_large" + p2 + ".jpg";
    });
    $(".popup_content img").attr("src", "");
    $(".popup_content img").parent().addClass("loading");
    $(".popup_content img").attr("src", newSrc);
    scrollAmount = body.scrollTop();
    overlay.addClass("visible");
    popupContainer.addClass("visible");
    body.addClass("popup");
    body.css('top', -scrollAmount);
    overlay.click(closeFcn);
  })
});
