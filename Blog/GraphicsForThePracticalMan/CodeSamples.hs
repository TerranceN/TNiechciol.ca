{-# LANGUAGE QuasiQuotes #-}

module Blog.GraphicsForThePracticalMan.CodeSamples
( transformBasisText
, combineBasisText
, changeOfBasisLinearText
, moveRightAddingText
, moveRightScalingText
, translateUsingShearText
) where

import Text.RawString.QQ
import Data.Char (isSpace)

transformBasisText = dropWhile isSpace [r|
// We can define how to transform a vector using a basis
function vTransform(v, basis) {
  // v.x * (xBasis.x, xBasis.y)
  // In our earlier example: 5 * (1, 0) = (5, 0)
  var xBasisPart = vScale(basis.xBasis, v.x);

  // v.y * (yBasis.x, yBasis.y)
  // In our earlier example: 3 * (0, 1) = (0, 3)
  var yBasisPart = vScale(basis.yBasis, v.y);

  // In our earlier example: (5, 0) + (0, 3) = (5, 3)
  return vAdd(xBasisPart, yBasisPart);
}

// And use that to draw triangles
function triWithBasis(basis, v1, v2, v3) {
  var vertices = [v1, v2, v3];
  for (var i = 0; i < vertices.length; i++) {
    vertices[i] = vTransform(vertices[i], basis);
  }
  tri(vertices[0], vertices[1], vertices[2]);
}

function starWithBasis(basis) {
  triWithBasis(basis, {x:-10, y:-10}, {x:  0, y:-30}, {x: 10, y:-10});
  triWithBasis(basis, {x: 10, y:-10}, {x: 30, y:  0}, {x: 10, y: 10});
  ...
}

...

var scaleBasis = {
  xBasis: {x: 1, y: 0},
  yBasis: {x: 0, y: 2}
};

starWithBasis(scaleBasis);
|]

combineBasisText = dropWhile isSpace [r|
function combineBases(basis1, basis2) {
  return {
    xBasis: vTransform(basis2.xBasis, basis1),
    yBasis: vTransform(basis2.yBasis, basis1)
  };
}

...

var rotationBasis = {
  xBasis: {x: Math.cos(angle), y: Math.sin(angle)},
  yBasis: {x: -Math.sin(angle), y: Math.cos(angle)}
};

var scaleBasis = {
  xBasis: {x: 1, y: 0},
  yBasis: {x: 0, y: 2}
};

starWithBasis(combineBases(rotationBasis, scaleBasis));
|]

changeOfBasisLinearText = dropWhile isSpace [r|
changeOfBasis(v1 + v2) = changeOfBasis(v1) + changeOfBasis(v2)
changeOfBasis(s*v1) = s*changeOfBasis(v1)
|]

moveRightAddingText = dropWhile isSpace [r|
moveRight(v1 + v2)            = {x: (v1.x+1) + (v2.x+1), y: (v1.y+v2.y)}
                              = {x: (v1.x+v2.x)+2, y: (v1.y+v2.y)}
//                                             /|\
//                                              |
// But is not equal to                          |
//                                              |
//                                             \|/
moveRight(v1) + moveRight(v2) = {x: (v1.x+v2.x)+1, y: (v1.y+v2.y)}
|]

moveRightScalingText = dropWhile isSpace [r|
s*moveRight(v1) = {x: s*(v1.x+1), y: (v1.y)}
                = {x: (s*v1.x)+s, y: (v1.y)}
//                            /|\
//                             |
// But is not equal to         |
//                             |
//                            \|/
moveRight(s*v1) = {x: (s*v1.x)+1, y: (v1.y)}
|]

translateUsingShearText = dropWhile isSpace [r|
var rotateBasis = {
  xBasis: {x: Math.cos(angle), y: Math.sin(angle), z: 0},
  yBasis: {x: -Math.sin(angle), y: Math.cos(angle), z: 0},
  zBasis: {x: 0, y: 0, z: 1},
};

var translateBasis = {
  xBasis: {x: 1, y: 0, z: 0},
  yBasis: {x: 0, y: 1, z: 0},
  zBasis: {x: mouseX, y: mouseY, z: 1},
};

starWithBasis(combineBases(translateBasis, rotateBasis));
|]
