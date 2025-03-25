# Interpolation

This example demonstrates movement interpolation and fixed-rate update functions.

### Updates

A fixed-rate update function runs every 50 milliseconds (20 times per second), moving a circle horizontally, bouncing back and forth.

### Interpolation

Because the update function doesn't run at the same frequency as the render function, the animation is not synchronized with the movement. This means that the "current" state when rendering is often slightly different from what it would be in a perfectly timed system.

`render` can use the `current`, `previous`, and `t` fields from the `States` record to estimate the correct position of the circle:

```purescript
type States a = { current :: a, previous :: a, t :: Number }
```

`t` is a number from `0` to `1` that represents a fraction of the fixed-rate update interval: the amount of time passed since the last update.

There are other methods of interpolation, but a simple linear interpolation function is exported by `Gesso.State`:

```purescript
lerp :: States Number -> Number
lerp { current, previous, t } = (1.0 - t) * previous + t * current
```

### Animation

A circle moves back and forth, leaving a trail. Each second, it alternates between:

1. a black circle moving with interpolated position
2. a white circle moving without interpolating its position

The white trail is choppier than the black, showing the difference that interpolation makes.

## Sample output

[See this example in action](https://smilack.github.io/purescript-gesso/examples/interpolation/dist/)

![A black circle moves to the right, leaving a smooth trail. After one second, it changes to a white circle and leaves a choppy trail. The black and white circles alternate each second, moving back and forth, demonstrating the difference between motion that has been smoothed and motion that hasn't.](interpolation.gif)
