export function getContext2DImpl(attributes, canvas) {
  console.log(attributes);
  return canvas.getContext("2d", attributes);
}
