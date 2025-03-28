export function _now() {
   return performance.now();
}

export function measureTextImpl(ctx, text) {
   return ctx.measureText(text);
}
