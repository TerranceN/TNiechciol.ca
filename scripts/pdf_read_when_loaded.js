// For electron-pdf. Tells electron when the page is ready to be turned in to a PDF

window.onload = function() {
  console.log("Ready!");
  var eventEmitInterval = setInterval(function () {
    document.body.dispatchEvent(new Event('view-ready'));
  }, 25);

  document.body.addEventListener('view-ready-acknowledged', function(){
    clearInterval(eventEmitInterval)
  });
}
