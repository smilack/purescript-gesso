export function getContext2DImpl(attributes, canvas) {
  return canvas.getContext("2d", attributes);
}
