exports.canvg = function(a,b) {
  return canvg.Canvg.fromString(a.getContext('2d'),b).start();
};
exports.mkZip = function() {
  return new JSZip();
};
exports.saveCanvas = function(canvas, filename, zip) {
  canvas.toBlob(function(blob) {
    console.log(blob);
    zip.file(filename+".png", blob);
  });
}
exports.saveZip = function(zip) {
  setTimeout(function() {
    console.log(zip);
    zip.generateAsync({type:"blob"})
    .then(function(content) {
      saveAs(content, "frames.zip");
    });
  }, 1000);
}
