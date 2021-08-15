# Bouncing Ball

Bouncing Ball builds on the [Quick Start Tutorial](https://github.com/smilack/purescript-gesso/blob/master/docs/tutorials/quick-start.md), adding animation and local state.

The imports in this example are still qualified, but aliased, unlike in the first tutorial. For example, `Gesso.Canvas` is now `GCan`.

## Output

Because this example does not use the `Scaler` functions (introduced in other examples), your output may look different depending on the size of your browser window.

![Hello example output](/examples/hello/output.png)

## The Code

The complete source for this example is located at [examples/bouncing-ball/src/Main.purs](https://github.com/smilack/purescript-gesso/blob/master/examples/bouncing-ball/src/Main.purs).

### 1. Adding State

The first change we will make is to create a type for our state. This needs to keep track of where the ball is (`x` and `y`) and what direction it's going (`vx` and `vy`). We will also put the size of the ball (`radius`) here too.

```purescript
type State =
  { x :: Number
  , vx :: Number
  , y :: Number
  , vy :: Number
  , radius :: Number
  }
```

To tell Gesso about the state, ...

### 2. Rendering

### 3. Updating