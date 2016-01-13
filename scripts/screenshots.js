$(function() {
  $(".screenshot").click(function() {
    var newSrc = $("img", $(this))[0].src.replace(/^(.*)_small.png$/, function(match, p1) {
      return p1 + "_large.png";
    });
    var overlay = $('<div class="overlay"></div>');
    var popup = $('<div class="popup_container"><div class="popup_content"><div class="screenshot_popup"><a href="javascript:none(0);" class="close">x</a><img src="' + newSrc + '"/></div></div></div>');
    $("body").append(overlay);
    $("body").append(popup);
    var closeFcn = function () {
      overlay.remove();
      popup.remove();
    }
    overlay.click(closeFcn);
    $(".close", popup).click(closeFcn);
  })
});
