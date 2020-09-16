"use strict"

exports._requestAnimationFrame = function(fn) {
  return function(window) {
    return function() {
      return window.requestAnimationFrame(function(time) {
        fn(time)();
      });
    };
  };
};