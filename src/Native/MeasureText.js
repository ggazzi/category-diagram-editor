var _user$project$Native_MeasureText = function() {

  var canvasContext = document.createElement('canvas').getContext('2d');

  var measureText = function(font, text) {
    canvasContext.font = font;
    return canvasContext.measureText(text).width;
  }

  return {
    'measureText': F2(measureText)
  };
}();
