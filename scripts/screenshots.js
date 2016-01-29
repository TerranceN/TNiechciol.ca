$(function() {
  var overlay = $(".overlay");
  var popupContainer = $(".popup_container");
  var body = $("body");
  $(".screenshot").click(function() {
    var newSrc = $("img", $(this))[0].src.replace(/^(.*)_small.jpg$/, function(match, p1) {
      return p1 + "_large.jpg";
    });
    var popup = $('<div class="screenshot_popup"><a href="javascript:none(0);" class="close">x</a><img src="' + newSrc + '"/></div>');
    var scrollAmount = body.scrollTop();
    $(".popup_content").html(popup);
    overlay.addClass("visible");
    popupContainer.addClass("visible");
    body.addClass("popup");
    body.css('top', -scrollAmount);
    var closeFcn = function () {
      overlay.removeClass("visible");
      popupContainer.removeClass("visible");
      body.css('top', 0);
      body.removeClass("popup");
      body.scrollTop(scrollAmount);
    }
    overlay.click(closeFcn);
    $(".close", popup).click(closeFcn);
  })
});
