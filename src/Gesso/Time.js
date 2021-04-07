"use strict"

exports._requestAnimationFrame = function (fn) {
  return function (window) {
    return function () {
      return window.requestAnimationFrame(fn);
    };
  };
};

exports.cancelAnimationFrame = function (id) {
  return function (window) {
    return function () {
      return window.cancelAnimationFrame(id);
    };
  };
};