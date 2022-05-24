export function _requestAnimationFrame(fn) {
  return function (window) {
    return function () {
      return window.requestAnimationFrame(fn);
    };
  };
};

export function cancelAnimationFrame(id) {
  return function (window) {
    return function () {
      return window.cancelAnimationFrame(id);
    };
  };
};