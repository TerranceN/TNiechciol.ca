var Module = (function() {
  var canvas, ctx, width, height;
	function setCanvas(canvasElement) {
  	if (canvasElement !== null) {
      canvas = canvasElement;
      ctx = canvas.getContext("2d");
      width = canvas.width;
      height = canvas.height;
      initialMouse = fromGridPoint({x: 5, y: 6});
      mouse = Object.assign({}, initialMouse);
    }
  }
  function getCanvas() {
    return canvas;
  }
  setCanvas(document.getElementById("canvas"));

	//var scale = 50;
  //var arrowHeadSize = 15;
	var scale = 25;
  var arrowHeadSize = 10;
  //var scale = 40;
  //var arrowHeadSize = 13;
  var arrowHeadAngle = Math.PI/6;
  var pointRadius = 3;
  
  var originOffset = 1.0;
  var initialMouse;
  var mouse = Object.assign({}, initialMouse);
  var rounded = {x: 0, y: 0};
  
  var updatePoint = function() {
    rounded = toGridPoint(mouse);
    rounded.x = Math.max(Math.round(rounded.x), 0);
    rounded.y = Math.max(Math.round(rounded.y/2)*2, 0);

    var element = canvas;
    while (element !== undefined && element.className.indexOf("canvas_demo") == -1) {
      element = element.parentNode;
    }
    if (element != undefined) {
      var xText = element.querySelector(".xtext");
      var yText = element.querySelector(".ytext");
      var finishedText = element.querySelector(".finished_text");

      if (xText)
        xText.innerText = "" + Math.round(rounded.x);
      if (yText)
        yText.innerText = "" + Math.round(rounded.y/2);
      if (finishedText)
        finishedText.innerText = "(" + rounded.x + ", " + rounded.y + ")";
    }
  }
  
  var onMouseMove = function(event) {
    mouse.x = event.layerX;
    mouse.y = event.layerY;
    updatePoint();
  }
  
  var onMouseLeave = function(event) {
    mouse = Object.assign({}, initialMouse);
    updatePoint();
  }

	function fromGridPoint(point) {
  	return {
    	x: (point.x+originOffset)*scale,
      y: height - (point.y+originOffset)*scale
    };
  }
  
  function toGridPoint(point) {
  	return {
    	x: point.x/scale-originOffset,
      y: -(point.y - height)/scale-originOffset
    };
  }
  
  function pointAdd(p1, p2) {
  	return {
    	x: p1.x+p2.x,
      y: p1.y+p2.y
    }
  }
  
  function pointScale(p1, s) {
  	return {
    	x: p1.x*s,
      y: p1.y*s
    }
  }
  
  function pointDiff(p1, p2) {
  	return {
    	x: p1.x-p2.x,
      y: p1.y-p2.y
    };
  }
  
  function pointFromAngle(a) {
  	return {
    	x: Math.cos(a),
      y: Math.sin(a)
    }
  }
  
  function drawArrowHead(end, angle) {
  	var arrowEnd1 = pointAdd(end, pointScale(pointFromAngle(angle+arrowHeadAngle), arrowHeadSize));
    var arrowEnd2 = pointAdd(end, pointScale(pointFromAngle(angle-arrowHeadAngle), arrowHeadSize));
    ctx.moveTo(end.x, end.y);
    ctx.lineTo(arrowEnd1.x, arrowEnd1.y);
    ctx.moveTo(end.x, end.y);
    ctx.lineTo(arrowEnd2.x, arrowEnd2.y);
  }
  
  function drawLine(start, end) {
  	ctx.moveTo(start.x, start.y);
    ctx.lineTo(end.x, end.y);
  }
  
  function drawGridLine(gridStart, gridEnd) {
  	var start = fromGridPoint(gridStart);
    var end = fromGridPoint(gridEnd);
  	ctx.beginPath();
    drawLine(start, end);
    ctx.stroke();
  }

	function drawArrow(gridStart, gridEnd) {
    var start = fromGridPoint(gridStart);
    var end = fromGridPoint(gridEnd);
    var diff = pointDiff(end, start);
    var angle = Math.atan2(-diff.y, -diff.x);
    
  	ctx.beginPath();
    drawLine(start, end);
    drawArrowHead(end, angle);
    ctx.stroke();
  }

  function drawGrid() {
    ctx.lineWidth = 1;
    ctx.strokeStyle='#DDDDDD';
    ctx.beginPath();
    for (var i = 0; i < height/scale; i++) {
    	ctx.moveTo(0, height - i*scale);
    	ctx.lineTo(width, height - i*scale);
    }
    for (var i = 0; i < width/scale; i++) {
    	ctx.moveTo(i*scale, 0);
    	ctx.lineTo(i*scale, height);
    }
    ctx.stroke();

    ctx.lineWidth = 3;
    ctx.strokeStyle='#888888';
    ctx.beginPath();
    ctx.moveTo(originOffset*scale, 0);
    ctx.lineTo(originOffset*scale, height);
    ctx.moveTo(0, height - originOffset*scale);
    ctx.lineTo(width, height - originOffset*scale);
    ctx.stroke();
    
  }

  function renderFiveThree() {
  }

  function render() {
    ctx.clearRect(0, 0, width, height);

    drawGrid();
    
    ctx.lineWidth = 2;
    ctx.strokeStyle='#880000';
    
    for (var i = 0; i < rounded.x; i++) {
      if (i == 0) {
        drawArrow({x: 0, y: 0}, {x: 1, y: 0});
      } else {
        drawArrow({x: i+0.15, y: 0}, {x: i+1, y: 0});
      }
    }
    //for (var i = 0; i < rounded.y; i++) {
    //  drawArrow({x: rounded.x, y: i+0.15}, {x: rounded.x, y: i+1});
    //}
    for (var i = 0; i < rounded.y; i+=2) {
      drawArrow({x: rounded.x, y: i+0.15}, {x: rounded.x, y: i+2});
    }
    /*
    drawArrow({x: 0, y: 0}, {x: 1, y: 0});
    drawArrow({x: 1.15, y: 0}, {x: 2, y: 0});
    drawArrow({x: 2.15, y: 0}, {x: 3, y: 0});
    drawArrow({x: 3.15, y: 0}, {x: 4, y: 0});
    drawArrow({x: 4.15, y: 0}, {x: 5, y: 0});
    drawArrow({x: 5, y: 0.15}, {x: 5, y: 2});
    drawArrow({x: 5, y: 2.15}, {x: 5, y: 4});
    drawArrow({x: 5, y: 4.15}, {x: 5, y: 5.85});
    */
    
    //drawArrow({x: 0, y: 0}, {x: 1.5, y: 0});
    //drawArrow({x: 0, y: 0}, {x: 0, y: 1.5});
    
    /*var angle = Math.PI/4;
    drawArrow({x: 0, y: 0}, {x: Math.cos(angle), y: Math.sin(angle)});
    drawArrow({x: 0, y: 0}, {x: -Math.sin(angle), y: Math.cos(angle)});*/
    
    /*
    drawArrow({x: 0, y: 0}, {x: 1, y: 0});
    drawArrow({x: 0, y: 0}, {x: toGridPoint({x: mouse.x, y: 0}).x, y: 1});
    */
    
    ctx.beginPath();
    var point = fromGridPoint(rounded);
    ctx.arc(point.x, point.y, pointRadius, 0, 2 * Math.PI, false);
    ctx.fillStyle = '#000000';
    ctx.fill();
    
    ctx.font="20px Georgia";
    ctx.fillStyle = '#000000';
		//ctx.fillText("(1, 0)",80,height-20);
    //ctx.fillText("(0, 1)",0,height-100);
    
    ctx.fillText("(" + rounded.x + ", " + rounded.y + ")",Math.max(point.x-50, 0),point.y-8);
    
    /*var xBasisTextY = height-140;
    var yBasisTextY = height-140;
    ctx.fillText("(cos(θ),",100,xBasisTextY);
    ctx.fillText("sin(θ))",110,xBasisTextY+22);
    ctx.fillText("(-sin(θ),",0,yBasisTextY);
    ctx.fillText("cos(θ))",10,xBasisTextY+22);
    
    ctx.fillText("θ",110,height-82);
    
    arrowHeadSize = 8;
    ctx.lineWidth = 2;
    ctx.strokeStyle='#000000';
    ctx.beginPath();
    
    var origin = fromGridPoint({x: 0, y: 0});
    var arcStart = fromGridPoint({x: 0.5, y: 0});
    var arcEnd = fromGridPoint({x: 0.5*Math.cos(angle), y: 0.5*Math.sin(angle)});
    ctx.arc(origin.x,origin.y,arcStart.x-origin.x,-Math.PI/4,0);
    drawArrowHead(arcEnd, angle+Math.PI/20);
    ctx.stroke();*/
    
    /*
    ctx.fillText("(1, 0)",120,height-60);
    ctx.fillText("(sx, 1)",mouse.x,height-130);
    */
    
    /*
    ctx.lineWidth = 4;
    ctx.strokeStyle='#000000';
    
    var gridMouseX = toGridPoint({x: mouse.x, y: 0}).x;
    drawGridLine({x: 0+gridMouseX, y: 1}, {x: 1+gridMouseX, y: 1});
    ctx.lineWidth = 2;
    drawGridLine({x: 0, y: 0}, {x: 1, y: 0});
    drawGridLine({x: 0, y: 0}, {x: 0+gridMouseX, y: 1});
    drawGridLine({x: 1, y: 0}, {x: 1+gridMouseX, y: 1});
    
    ctx.lineWidth = 2;
    ctx.strokeStyle='#AA0000';
    
    drawArrow({x: 0+gridMouseX, y: 1}, {x: 0+gridMouseX, y: 0});
    drawArrow({x: 1+gridMouseX, y: 1}, {x: 1+gridMouseX, y: 0});
    
    ctx.beginPath();
    {
      var point1 = fromGridPoint({x: 0+gridMouseX, y: 1});
      var point2 = fromGridPoint({x: 1+gridMouseX, y: 1});
      ctx.arc(point1.x, point1.y, pointRadius, 0, 2 * Math.PI, false);
      ctx.arc(point2.x, point2.y, pointRadius, 0, 2 * Math.PI, false);
    }
    ctx.fillStyle = '#CC0000';
    ctx.fill();
    */
  }
  
  var drawLoop = DrawLoop(render);

	function start() {
    updatePoint();
    canvas.addEventListener("mousemove", onMouseMove);
    canvas.addEventListener("mouseleave", onMouseLeave);
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
