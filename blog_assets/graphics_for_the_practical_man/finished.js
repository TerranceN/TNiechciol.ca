var Module = (function() {
  var canvas, ctx, width, height, depthBuffer;
  function setCanvas(canvasElement) {
    if (canvasElement !== null) {
      canvas = canvasElement;
      ctx = canvas.getContext("2d");
      width = canvas.width;
      height = canvas.height;
      depthBuffer = Array(width*height).fill(0);
    }
  }
  function getCanvas() {
    return canvas;
  }
  setCanvas(document.getElementById("canvas"));

  function updateDepth(x, y, z) {
    if (z < 0 || z > 1) {
      return false;
    }
    var index = Math.round(x)+Math.round(y)*width;
    if (z < depthBuffer[index]) {
      depthBuffer[index] = z;
      return true;
    }
    return false;
  }

  function fillPixel(x, y) {
    ctx.fillRect(Math.round(x), Math.round(y), 1, 1);
  }

  function tri(p1, p2, p3) {
    //console.log(p3.z);
    function halfTri(p1, p2, p3) {
      var avgZ = (p1.z + p2.z + p3.z)/3;
      var invslope1 = (p2.x - p1.x) / (p2.y - p1.y);
      var invslope2 = (p3.x - p1.x) / (p2.y - p1.y);
      var zslope2 = (p2.z - p1.z) / (p2.y - p1.y);
      var zslope3 = (p3.z - p1.z) / (p3.y - p1.y);
      var curx1 = p2.x;
      var curx2 = p3.x;
      var dir = 1;
      if (p2.y < p1.y) {
        dir = -1;
      }
      var lines = Math.abs(p2.y - p1.y);
      for (var line = 0; line < lines; line++) {
        var z2 = p2.z - line*dir*zslope2;
        var z3 = p3.z - line*dir*zslope3;
        var lineWidth = Math.abs(curx2-curx1);
        // draw a line from curx1 to curx2
        for (var pixelx = 0; pixelx < lineWidth; pixelx++) {
          var x = pixelx + Math.min(curx1, curx2);
          var y = p2.y-line*dir;
          var zslopex = (z3 - z2) / (curx2 - curx1);
          var z;
          if (p3.x > p2.x) {
            z = z2 + pixelx*zslopex;
          } else {
            z = z3 + pixelx*zslopex;
          }
          if (updateDepth(x, y, z)) {
            fillPixel(x, y);
          }
        }
        curx1 -= invslope1*dir;
        curx2 -= invslope2*dir;
      }
    }
    var points = [p1, p2, p3].sort(function(a, b) {
      return a.y - b.y;
    });
    points[0].y = Math.floor(points[0].y);
    points[1].y = Math.round(points[1].y);
    points[2].y = Math.ceil(points[2].y);
    var invslope =  (points[2].x-points[0].x)/(points[2].y-points[0].y);
    var invslopez = (points[2].z-points[0].z)/(points[2].x-points[0].x);
    var other = Object.assign({}, points[1]);
    other.x = points[0].x + invslope *(points[1].y-points[0].y);
    other.z = points[0].z + invslopez*(other.x-points[0].x);
    points[0].x = Math.round(points[0].x);
    points[2].x = Math.round(points[2].x);
    if (other.x > points[1].x) {
      points[1].x = Math.floor(points[1].x);
      other.x = Math.ceil(other.x);
    } else {
      points[1].x = Math.ceil(points[1].x);
      other.x = Math.floor(other.x);
    }
    if (points[1].y != points[0].y) {
      halfTri(points[0], points[1], other);
    }
    if (points[1].y != points[2].y) {
      halfTri(points[2], points[1], other);
    }
  }

  function starAt(x, y) {
    tri({x:-10+x, y:-10+y}, {x:  0+x, y:-30+y}, {x: 10+x, y:-10+y});
    tri({x: 10+x, y:-10+y}, {x: 30+x, y:  0+y}, {x: 10+x, y: 10+y});
    tri({x:-10+x, y: 10+y}, {x:  0+x, y: 30+y}, {x: 10+x, y: 10+y});
    tri({x:-10+x, y:-10+y}, {x:-30+x, y:  0+y}, {x:-10+x, y: 10+y});
    tri({x:-10+x, y:-10+y}, {x: 10+x, y:-10+y}, {x: 10+x, y: 10+y});
    tri({x:-10+x, y:-10+y}, {x: 10+x, y: 10+y}, {x:-10+x, y: 10+y});
  }

  function starAtRot(x, y, a) {
    var c = Math.cos(a);
    var s = Math.sin(a);
    tri(
      {x:c*-10-s*-10+x, y:s*-10+c*-10+y},
      {x:c*  0-s*-30+x, y:s*  0+c*-30+y},
      {x:c* 10-s*-10+x, y:s* 10+c*-10+y}
    );
    tri(
      {x:c* 10-s*-10+x, y:s* 10+c*-10+y},
      {x:c* 30-s*  0+x, y:s* 30+c*  0+y},
      {x:c* 10-s* 10+x, y:s* 10+c* 10+y}
    );
    tri(
      {x:c*-10-s* 10+x, y:s*-10+c* 10+y},
      {x:c*  0-s* 30+x, y:s*  0+c* 30+y},
      {x:c* 10-s* 10+x, y:s* 10+c* 10+y}
    );
    tri(
      {x:c*-10-s*-10+x, y:s*-10+c*-10+y},
      {x:c*-30-s*  0+x, y:s*-30+c*  0+y},
      {x:c*-10-s* 10+x, y:s*-10+c* 10+y}
    );
    tri(
      {x:c*-10-s*-10+x, y:s*-10+c*-10+y},
      {x:c* 10-s*-10+x, y:s*10+c* -10+y},
      {x:c* 10-s* 10+x, y:s*10+c*  10+y}
    );
    tri(
      {x:c*-10-s*-10+x, y:s*-10+c*-10+y},
      {x:c* 10-s* 10+x, y:s* 10+c* 10+y},
      {x:c*-10-s* 10+x, y:s*-10+c* 10+y}
    );
  }

  function transformPoint(p, t) {
    if (t.type == 'translation') {
      return {
        x: p.x + t.x,
        y: p.y + t.y
      }
    } else if (t.type == 'rotation') {
      return {
        x: t.cos*p.x-t.sin*p.y,
        y: t.sin*p.x+t.cos*p.y
      };
    }
  }

  function triTransformed(transforms, v1, v2, v3) {
    var t1 = v1, t2 = v2, t3 = v3;
    for (var i = 0; i < transforms.length; i++) {
      var t = transforms[i];
      t1 = transformPoint(t1, t);
      t2 = transformPoint(t2, t);
      t3 = transformPoint(t3, t);
    }
    tri(t1, t2, t3);
  }

  function starTransformed(transforms) {
    triTransformed(transforms, {x:-10, y:-10}, {x:  0, y:-30}, {x: 10, y:-10});
    triTransformed(transforms, {x: 10, y:-10}, {x: 30, y:  0}, {x: 10, y: 10});
    triTransformed(transforms, {x:-10, y: 10}, {x:  0, y: 30}, {x: 10, y: 10});
    triTransformed(transforms, {x:-10, y:-10}, {x:-30, y:  0}, {x:-10, y: 10});
    triTransformed(transforms, {x:-10, y:-10}, {x: 10, y:-10}, {x: 10, y: 10});
    triTransformed(transforms, {x:-10, y:-10}, {x: 10, y: 10}, {x:-10, y: 10});
  }

  function translate(x, y) {
    return {
      type: 'translation',
      x: x,
      y: y
    }
  }

  function rotate(a) {
    return {
      type: 'rotation',
      angle: a,
      cos: Math.cos(a),
      sin: Math.sin(a)
    }
  }

  function vAdd(v1, v2) {
    return {
      x: v1.x + v2.x,
      y: v1.y + v2.y,
      z: v1.z + v2.z
    };
  }

  function vScale(v, s) {
    return {
      x: v.x * s,
      y: v.y * s,
      z: v.z * s
    };
  }

  function vDiff(v1, v2) {
    return {
      x: v1.x - v2.x,
      y: v1.y - v2.y,
      z: v1.z - v2.z,
    };
  }

  function vCross(v1, v2) {
    return {
      x: v1.y*v2.z - v1.z*v2.y,
      y: v1.z*v2.x - v1.x*v2.z,
      z: v1.x*v2.y - v1.y*v2.x
    };
  }

  function vDot(v1, v2) {
    return v1.x*v2.x + v1.y*v2.y + v1.z*v2.z;
  }

  function vLength(v) {
    return Math.sqrt(vDot(v, v));
  }

  function vNormalize(v) {
    return vScale(v, 1/vLength(v));
  }

  function vTransform(v, basis) {
    var xBasisPart = vScale(basis.xBasis, v.x);
    var yBasisPart = vScale(basis.yBasis, v.y);
    return vAdd(xBasisPart, yBasisPart);
  }

  function triWithBasis(basis, v1, v2, v3) {
    var vertices = [v1, v2, v3];
    for (var i = 0; i < vertices.length; i++) {
      vertices[i] = vTransform(vertices[i], basis);
    }
    tri(vertices[0], vertices[1], vertices[2]);
  }

  function starWithBasis(basis) {
    triWithBasis(basis, {x:-10, y:-10}, {x:  0, y:-30}, {x:10, y:-10});
    triWithBasis(basis, {x: 10, y:-10}, {x: 30, y:  0}, {x: 10, y:10});
    triWithBasis(basis, {x:-10, y: 10}, {x:  0, y: 30}, {x: 10, y:10});
    triWithBasis(basis, {x:-10, y:-10}, {x:-30, y:  0}, {x:-10, y:10});
    triWithBasis(basis, {x:-10, y:-10}, {x: 10, y:-10}, {x: 10, y:10});
    triWithBasis(basis, {x:-10, y:-10}, {x: 10, y: 10}, {x:-10, y:10});
  }


  function combineBases(basis1, basis2) {
    return {
      xBasis: vTransform(basis2.xBasis, basis1),
      yBasis: vTransform(basis2.yBasis, basis1)
    };
  }

  function combineBasesList(lst) {
    var basis = lst[0];
    for (var i = 1; i < lst.length; i++) {
      basis = combineBases(basis, lst[i]);
    }
    return basis;
  }

  function starWithBases(basisLst) {
    starWithBasis(combineBasesList(basisLst));
  }

  function mTransform(matrix, v) {
    var res = Array(4);
    var vert = [v.x, v.y, v.z, 1];
    for (var j = 0; j < 4; j++) {
      var total = 0;
      for (var k = 0; k < 4; k++) {
        total += matrix[j*4+k] * vert[k];
      }
      res[j] = total;
    }
    return {
      x: res[0],
      y: res[1],
      z: res[2],
      w: res[3],
    };
  }

  function mMult(mat1, mat2) {
    var res = Array(16);
    for (var i = 0; i < 4; i++) {
      for (var j = 0; j < 4; j++) {
        var total = 0;
        for (var k = 0; k < 4; k++) {
          total += mat1[j*4+k] * mat2[k*4+i];
        }
        res[j*4+i] = total;
      }
    }
    return res;
  }

  function triWithMatrix(matrix, v1, v2, v3) {
    var vertices = [v1, v2, v3];
    for (var i = 0; i < vertices.length; i++) {
      vertices[i].z = 0;
      vertices[i] = mTransform(matrix, vertices[i]);
      vertices[i].x /= vertices[i].w;
      vertices[i].y /= vertices[i].w;
      vertices[i].z /= vertices[i].w;
      vertices[i].x = vertices[i].x * 30 + 100;
      vertices[i].y = vertices[i].y * 30 + 100;
    }
    tri(vertices[0], vertices[1], vertices[2]);
  }

  var color = [1, 1, 1];

  function triWithMatrices(perspectiveMatrix, modelViewMatrix, v1, v2, v3) {
    var vertices = [v1, v2, v3];
    for (var i = 0; i < vertices.length; i++) {
      if (typeof(vertices[i].z) === 'undefined') {
        vertices[i].z = 0;
      }
      vertices[i] = mTransform(modelViewMatrix, vertices[i]);
    }
    var normal = vNormalize(vCross(
      vDiff(vertices[1], vertices[2]),
      vDiff(vertices[0], vertices[1])));
    var dot = Math.abs(vDot({x: 0, y: 0, z: 1}, normal));
    var colorStr = '#';
    for (var i = 0; i < 3; i++) {
      var str = Math.round((dot * 0.6 * color[i]) * 255).toString(16);
      if (str.length < 2) {
        str = '0' + str;
      }
      colorStr += str;
    }
    ctx.fillStyle = colorStr;
    for (var i = 0; i < vertices.length; i++) {
      vertices[i] = vAdd(vertices[i], vScale(normal, -0));
      vertices[i] = mTransform(perspectiveMatrix, vertices[i]);
      vertices[i].x /= vertices[i].w;
      vertices[i].y /= vertices[i].w;
      vertices[i].z /= vertices[i].w;
      vertices[i].x = vertices[i].x * width/2 + width/2;
      vertices[i].y = vertices[i].y * height/2 + height/2;
    }
    tri(vertices[0], vertices[1], vertices[2]);
  }

  function starWithMatrix(matrix) {
    triWithMatrix(matrix, {x:-10, y:-10}, {x:  0, y:-30}, {x:10, y:-10});
    triWithMatrix(matrix, {x: 10, y:-10}, {x: 30, y:  0}, {x: 10, y:10});
    triWithMatrix(matrix, {x:-10, y: 10}, {x:  0, y: 30}, {x: 10, y:10});
    triWithMatrix(matrix, {x:-10, y:-10}, {x:-30, y:  0}, {x:-10, y:10});
    triWithMatrix(matrix, {x:-10, y:-10}, {x: 10, y:-10}, {x: 10, y:10});
    triWithMatrix(matrix, {x:-10, y:-10}, {x: 10, y: 10}, {x:-10, y:10});
  }

  function starWithMatrices(perspectiveMatrix, modelViewMatrix) {
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-10, y:-10}, {x:  0, y:-30}, {x:10, y:-10});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x: 10, y:-10}, {x: 30, y:  0}, {x: 10, y:10});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-10, y: 10}, {x:  0, y: 30}, {x: 10, y:10});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-10, y:-10}, {x:-30, y:  0}, {x:-10, y:10});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-10, y:-10}, {x: 10, y:-10}, {x: 10, y:10});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-10, y:-10}, {x: 10, y: 10}, {x:-10, y:10});
  }

  function squareWithMatrices(perspectiveMatrix, modelViewMatrix) {
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-30, y:-30}, {x: 30, y:-30}, {x: 30, y: 30});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x: 30, y: 30}, {x:-30, y: 30}, {x:-30, y:-30});
  }

  function cubeWithmatrices(perspectiveMatrix, modelViewMatrix) {
    color = [1, 0, 0];
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-30, y:-30, z:-30}, {x: 30, y:-30, z:-30}, {x: 30, y: 30, z:-30});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x: 30, y: 30, z:-30}, {x:-30, y: 30, z:-30}, {x:-30, y:-30, z:-30});

    color = [1, 0, 0];
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-30, y:-30, z: 30}, {x: 30, y: 30, z: 30}, {x: 30, y:-30, z: 30});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x: 30, y: 30, z: 30}, {x:-30, y:-30, z: 30}, {x:-30, y: 30, z: 30});

    color = [0, 1, 0];
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x: 30, y:-30, z:-30}, {x: 30, y:-30, z: 30}, {x: 30, y: 30, z: 30});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x: 30, y: 30, z: 30}, {x: 30, y: 30, z:-30}, {x: 30, y:-30, z:-30});

    color = [0, 1, 0];
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-30, y:-30, z:-30}, {x:-30, y: 30, z: 30}, {x:-30, y:-30, z: 30});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-30, y: 30, z: 30}, {x:-30, y:-30, z:-30}, {x:-30, y: 30, z:-30});

    color = [0, 0, 1];
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-30, y:-30, z:-30}, {x:-30, y:-30, z: 30}, {x: 30, y:-30, z: 30});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x: 30, y:-30, z: 30}, {x: 30, y:-30, z:-30}, {x:-30, y:-30, z:-30});

    color = [0, 0, 1];
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x:-30, y: 30, z:-30}, {x: 30, y: 30, z: 30}, {x:-30, y: 30, z: 30});
    triWithMatrices(perspectiveMatrix, modelViewMatrix, {x: 30, y: 30, z: 30}, {x:-30, y: 30, z:-30}, {x: 30, y: 30, z:-30});
  }

  function perspectiveMatrix(fovy, near, far) {
    var cot = 1/Math.tan(fovy/2);
    var aspect = width/height;
    var zdiff = near-far;
    return [
      cot/aspect,   0,                 0,                  0,
               0, cot,                 0,                  0,
               0,   0,  (near+far)/zdiff,  2*near*far/zdiff,
               0,   0,                -1,                  0,
    ];
  }

  var angle = 0;

  function render(dt) {
    ctx.clearRect(0, 0, width, height);
    depthBuffer.fill(1);
    ctx.strokeStyle='#000000';
    ctx.lineWidth = 2; // Make line width 2 to avoid gaps in the triangles

    /*
    tri(50, 80, 100, 150, 150, 100);

    ctx.strokeStyle='#FF0000';
    */

    /*
    tri(100, 100, 110, 80, 120, 100);
    tri(120, 100, 140, 110, 120, 120);
    tri(100, 120, 110, 140, 120, 120);
    tri(100, 100, 80, 110, 100, 120);
    tri(100, 100, 120, 100, 120, 120);
    tri(100, 100, 120, 120, 100, 120);
    */

    /*
    tri(-10, -10, 0, -30, 10, -10);
    tri(10, -10, 30, 0, 10, 10);
    tri(-10, 10, 0, 30, 10, 10);
    tri(-10, -10, -30, 0, -10, 10);
    tri(-10, -10, 10, -10, 10, 10);
    tri(-10, -10, 10, 10, -10, 10);
    */

    /*
    starAt(30, 170);

    starAtRot(40, 40, 2*angle);

    var transforms = [rotate(angle), translate(110, 110)];
    for (var i = 0; i < 4; i++) {
      starTransformed([translate(30*(2*(i%2)-1), 30*(2*Math.floor(i/2)-1))].concat(transforms));
    }

    starTransformed(transforms);

    var rotationBasis = {
      xBasis: {x: Math.cos(angle), y: Math.sin(angle)},
      yBasis: {x: -Math.sin(angle), y: Math.cos(angle)}
    };

    var scaleBasis = {
      xBasis: {x: 1, y: 0},
      yBasis: {x: 0, y: 2}
    };

    //starWithBasis(combineBases(rotationBasis, scaleBasis));

    starWithBases([rotationBasis, scaleBasis]);
    */

    var identityMatrix = [
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1,
    ];

    var rotationMatrixZ = [
      Math.cos(angle/3), -Math.sin(angle/3), 0, 0,
      Math.sin(angle/3),  Math.cos(angle/3), 0, 0,
                    0,                0, 1, 0,
                    0,                0, 0, 1,
    ];

    var rotationMatrixX = [
      1,               0,                0, 0,
      0, Math.cos(angle/2), -Math.sin(angle/2), 0,
      0, Math.sin(angle/2),  Math.cos(angle/2), 0,
      0,               0,                0, 1,
    ];

    var rotationMatrixY = [
       Math.cos(angle), 0, Math.sin(angle), 0,
                     0, 1,               0, 0,
      -Math.sin(angle), 0, Math.cos(angle), 0,
                     0, 0,               0, 1,
    ];

    var scaleMatrix = [
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, 0,
      0, 0, 0, 1,
    ];

    var translationMatrix = [
      1, 0, 0, 0,
      0, 1, 0, 0,
      0, 0, 1, -120,
      0, 0, 0, 1,
    ];

    var matrix = identityMatrix;
    matrix = mMult(scaleMatrix, matrix);
    matrix = mMult(rotationMatrixZ, matrix);
    matrix = mMult(rotationMatrixX, matrix);
    matrix = mMult(rotationMatrixY, matrix);
    matrix = mMult(translationMatrix, matrix);

    cubeWithmatrices(perspectiveMatrix(90*Math.PI/180, 1, 200), matrix);

    angle += 0.0015 * dt;
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
