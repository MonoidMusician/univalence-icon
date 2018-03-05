exports.canvg = canvg;
exports.saveCanvas = function(canvas, filename) {
  canvas.toBlob(function(blob) {
    saveAs(blob, filename);
  });
}
