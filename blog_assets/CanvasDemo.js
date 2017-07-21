var CanvasDemoLoader = (function() {
  function loadJs(scriptSrc) {
    return new Promise(function(resolve, reject) {
      var script = document.createElement('script');
      script.onload = resolve;
      script.src = scriptSrc;
      document.head.appendChild(script);
    });
  }

  function loadDemo(demo) {
    var promise = new Promise(function(resolve, reject) {
    });
    var src = demo.getAttribute("src");
    return loadJs(src).then(function() {
      var Demo = Module;
      Demo.setCanvas(demo);
      if (typeof(Demo.start) === 'function') {
        Demo.start();
      } else if (typeof(Demo.render) === 'function') {
        Demo.render();
      } else {
        console.warn("Demo has no 'start' or 'render' method! " + src);
      }
      return Demo;
    });
  }

  function loadAllDemos() {
    var demos = document.querySelectorAll(".canvas_demo canvas");
    return Promise.all([].map.call(demos, loadDemo));
  }

  return {
    loadDemo: loadDemo,
    loadAllDemos: loadAllDemos
  };
})();

var CanvasDemoToggler = (function() {
  function setupToggle(demo) {
    if (typeof(demo.start) === 'function' &&
        typeof(demo.stop) === 'function') {
      // Stop the demo for now, it'll automatically start when it's in view
      demo.stop();
      $(demo.getCanvas()).on('inview', function(event, inView) {
        if (inView) {
          demo.start()
        } else {
          demo.stop();
        }
      });
    }
  }

  return {
    setupToggle: setupToggle
  };
})();

function DrawLoop(renderFcn) {
  var isRunning = false;
  var then;
  function loop(now) {
    if (!then) {
      then = now;
    }
    var delta = now - then; then = now;
    renderFcn(Math.min(delta, 30));
    if (isRunning) {
      window.requestAnimationFrame(loop);
    }
  }
  function start() {
    if (!isRunning) {
      isRunning = true;
      window.requestAnimationFrame(loop);
    }
    return this;
  }
  function stop() {
    isRunning = false;
    return this;
  }
  return {
    start: start,
    stop: stop
  }
}

window.addEventListener('load', function() {
  CanvasDemoLoader.loadAllDemos().then(function(demos) {
    demos.forEach(CanvasDemoToggler.setupToggle)
  });
});
