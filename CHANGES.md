# Change Log

## v1.2.0

### Features

- (Issue #46) Provide function to access timestamp value from `performance.now`

## v1.1.1

### Features
- (Issue #23) Pause Gesso applications while page is hidden

### Bugs

- (Issue #62) Fixed: crash related to applications running in the background for a long time while page is hidden

## v1.1.0

### Features
- (Issue #40) Added box model dimensions:
  - `Boxed` and `Box` types (counterparts to `Rectangular` and `Rect`)
  - `box`, `top`, `right`, `bottom`, and `left` fields to `Scaler`
- Added many fields to `all` scaling function:
  - (Issue #40) Box model: `top`, `right`, `bottom`, `left`
  - (Issue #52) Gradients and curves: `x0`, `cpx`, `cp1x`, `cp2x`, `y0`, `cpy`, `cp1y`, `cp2y`, `r0`, `r1`
  - (Issue #47) Primes (`'`) and other numbered points/radii: `x'`, `x3`, `x4`, `x5`, `x6`, `x7`, `x8`, `x9`, `y'`, `y3`, `y4`, `y5`, `y6`, `y7`, `y8`, `y9`, `r'`, `r2`, `r3`, `r4`, `r5`, `r6`, `r7`, `r8`, `r9`
- (Issue #51) Made scaling operators associative
- (Issue #51) Added `compose` function for scalers
- (Issues #50, #54) Added `mkReferenceFrame` functions to create scalers between arbitrary areas (with optional aspect ratio preservation)

### Examples
- Added [reference frames example](examples/reference-frames)

## v1.0.1

### Bugs
- (Issue #55) Fixed: when multiple updates or interaction events happened in one frame, they were applied in reverse

### Examples
- Added [timing example](examples/timing) comparing delta-t in fixed-rate and per-frame update functions

