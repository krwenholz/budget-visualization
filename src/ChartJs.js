/* exports */
"use strict";

exports.drawChart = function(chartId, definition) {
  return function() {
    console.log("\n\n\nI received something awesome: " + chartId);
    console.log("\n\n\nI received something awesome: " + definition);
  };
};
