var _ggazzi$category_diagram_editor$Native_MeasureText = function() {

  var canvasContext = document.createElement('canvas').getContext('2d');

  var measureText = function(font, text) {
    canvasContext.font = font;
    return canvasContext.measureText(text).width;
  }

  return {
    'measureText': F2(measureText)
  };
}();
