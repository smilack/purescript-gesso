# Change Log

## v1.3.0
- [#34](https://github.com/smilack/purescript-gesso/issues/34) Added interaction functions for  `beforeinput`, `contextmenu`, `gotpointercapture`, `lostpointercapture`, `pointercancel`, `pointerdown`, `pointerenter`, `pointerleave`, `pointermove`, `pointerout`, `pointerover`, and `pointerup` events

## v1.2.0

### Features

- [#46](https://github.com/smilack/purescript-gesso/issues/46) Provide function to access timestamp value from `performance.now`

## v1.1.1

### Features
- [#23](https://github.com/smilack/purescript-gesso/issues/23) Pause Gesso applications while page is hidden

### Bugs

- [#62](https://github.com/smilack/purescript-gesso/issues/62) Fixed: crash related to applications running in the background for a long time while page is hidden

## v1.1.0

### Features
- [#40](https://github.com/smilack/purescript-gesso/issues/40) Added box model dimensions:
  - `Boxed` and `Box` types (counterparts to `Rectangular` and `Rect`)
  - `box`, `top`, `right`, `bottom`, and `left` fields to `Scaler`
- Added many fields to `all` scaling function:
  - [#40](https://github.com/smilack/purescript-gesso/issues/40) Box model: `top`, `right`, `bottom`, `left`
  - [#52](https://github.com/smilack/purescript-gesso/issues/52) Gradients and curves: `x0`, `cpx`, `cp1x`, `cp2x`, `y0`, `cpy`, `cp1y`, `cp2y`, `r0`, `r1`
  - [#47](https://github.com/smilack/purescript-gesso/issues/47) Primes (`'`) and other numbered points/radii: `x'`, `x3`, `x4`, `x5`, `x6`, `x7`, `x8`, `x9`, `y'`, `y3`, `y4`, `y5`, `y6`, `y7`, `y8`, `y9`, `r'`, `r2`, `r3`, `r4`, `r5`, `r6`, `r7`, `r8`, `r9`
- [#51](https://github.com/smilack/purescript-gesso/issues/51) Made scaling operators associative
- [#51](https://github.com/smilack/purescript-gesso/issues/51) Added `compose` function for scalers
- (Issues #50, #54) Added `mkReferenceFrame` functions to create scalers between arbitrary areas (with optional aspect ratio preservation)

### Examples
- Added [reference frames example](examples/reference-frames)

## v1.0.1

### Bugs
- [#55](https://github.com/smilack/purescript-gesso/issues/55) Fixed: when multiple updates or interaction events happened in one frame, they were applied in reverse

### Examples
- Added [timing example](examples/timing) comparing delta-t in fixed-rate and per-frame update functions

