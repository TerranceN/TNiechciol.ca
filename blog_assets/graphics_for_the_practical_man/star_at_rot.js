var Module = (function() {
  var canvas, ctx, width, height;
	function setCanvas(canvasElement) {
  	if (canvasElement !== null) {
      canvas = canvasElement;
      ctx = canvas.getContext("2d");
      width = canvas.width;
      height = canvas.height;
    }
  }
	function getCanvas() {
    return canvas;
  }
  setCanvas(document.getElementById("canvas"));

function tri(p1, p2, p3) {
    function halfTri(x1, y1, x2, x3, y23) {
      if (x2 < x3) {
        x2 = Math.floor(x2);
        x3 = Math.ceil(x3);
      } else {
        x2 = Math.ceil(x2);
        x3 = Math.floor(x3);
      }
      var invslope1 = (x2 - x1) / (y23 - y1);
      var invslope2 = (x3 - x1) / (y23 - y1);

      var curx1 = x2;
      var curx2 = x3;

      var dir = 1;
      if (y23 < y1) {
        dir = -1;
      }

      var lines = Math.abs(y23 - y1);

      for (var line = 0; line < lines; line++) {
      	var lineWidth = Math.abs(curx2-curx1);
        // draw a line from curx1 to curx2
      	for (var pixelx = 0; pixelx < lineWidth; pixelx++) {
        	var x = pixelx + Math.min(curx1, curx2);
        	ctx.fillRect(Math.round(x), y23-line*dir, 1, 1);
        }
        curx1 -= invslope1*dir;
        curx2 -= invslope2*dir;
      }
    }

    var tmp;
    var low = p1;
    var mid = p2;
    var high = p3

    if (mid.y < low.y) {
      tmp = low; low = mid; mid = tmp;
    }
    if (high.y < low.y) {
      tmp = low; low = high; high = tmp;
    }
    if (high.y < mid.y) {
      tmp = mid; mid = high; high = tmp;
    }
    low.y = Math.floor(low.y);
    mid.y = Math.round(mid.y);
    high.y = Math.ceil(high.y);
    var invslope = (high.x-low.x)/(high.y-low.y);
    var otherx = low.x + invslope*(mid.y-low.y);
    if (mid.y != low.y) {
      halfTri(low.x, low.y, mid.x, otherx, mid.y);
    }
    if (mid.y != high.y) {
      halfTri(high.x, high.y, mid.x, otherx, mid.y);
    }
  }

  function starAtRot(p, a) {
    var c = Math.cos(a);
    var s = Math.sin(a);
    tri(
      {x:c*-10-s*-10+p.x, y:s*-10+c*-10+p.y},
      {x:c*  0-s*-30+p.x, y:s*  0+c*-30+p.y},
      {x:c* 10-s*-10+p.x, y:s* 10+c*-10+p.y}
    );
    tri(
      {x:c* 10-s*-10+p.x, y:s* 10+c*-10+p.y},
      {x:c* 30-s*  0+p.x, y:s* 30+c*  0+p.y},
      {x:c* 10-s* 10+p.x, y:s* 10+c* 10+p.y}
    );
    tri(
      {x:c*-10-s* 10+p.x, y:s*-10+c* 10+p.y},
      {x:c*  0-s* 30+p.x, y:s*  0+c* 30+p.y},
      {x:c* 10-s* 10+p.x, y:s* 10+c* 10+p.y}
    );
    tri(
      {x:c*-10-s*-10+p.x, y:s*-10+c*-10+p.y},
      {x:c*-30-s*  0+p.x, y:s*-30+c*  0+p.y},
      {x:c*-10-s* 10+p.x, y:s*-10+c* 10+p.y}
    );
    tri(
      {x:c*-10-s*-10+p.x, y:s*-10+c*-10+p.y},
      {x:c* 10-s*-10+p.x, y:s*10+c* -10+p.y},
      {x:c* 10-s* 10+p.x, y:s*10+c*  10+p.y}
    );
    tri(
      {x:c*-10-s*-10+p.x, y:s*-10+c*-10+p.y},
      {x:c* 10-s* 10+p.x, y:s* 10+c* 10+p.y},
      {x:c*-10-s* 10+p.x, y:s*-10+c* 10+p.y}
    );
  }

  var angle = 0;

  function render(dt) {
    ctx.clearRect(0, 0, width, height);
    ctx.strokeStyle='#000000';
    ctx.lineWidth = 2; // Make line width 2 to avoid gaps in the triangles
    
    starAtRot({x: 100, y: 100}, angle);

    angle += 0.002 * dt;
  }
  
  var drawLoop = DrawLoop(render);

	function start() {
  	drawLoop.start();
    return this;
  }
	
  function stop() {
  	drawLoop.stop();
    return this;
  }
  
  return {
  	setCanvas: setCanvas,
    getCanvas: getCanvas,
  	start: start,
    stop: stop,
    render: render
  };
})();
