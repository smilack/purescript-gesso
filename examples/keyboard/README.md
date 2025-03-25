# Keyboard

This example uses keyboard events to move a blue square around the page. Control the square with the arrow keys.

### Interactions

This example modifies the default interactions record in `Gesso.Application.defaultBehavior` by adding two keyboard event handlers:

```purescript
, behavior: GApp.defaultBehavior
    { render = render
    , update = update
    , interactions { keyboard = [ keyDown, keyUp ] }
    }
```

The event handlers have type `Gesso.Interactions.KeyboardInteraction State`. They're constructed using `on` functions from `Gesso.Interactions`. They both listen for key events, and update the state when an arrow key is pressed or released:

```purescript
keyDown :: GInt.KeyboardInteraction State
keyDown = GInt.onKeyDown $ setKey true

keyUp :: GInt.KeyboardInteraction State
keyUp = GInt.onKeyUp $ setKey false

setKey :: Boolean -> KeyboardEvent -> Delta -> Scalers -> State -> Effect (Maybe State)
setKey val event _ _ state = pure $ case KEv.key event of
  "ArrowUp" -> Just state { keys { up = val } }
  "ArrowDown" -> Just state { keys { down = val } }
  "ArrowLeft" -> Just state { keys { left = val } }
  "ArrowRight" -> Just state { keys { right = val } }
  _ -> Nothing
```

## Sample output

[See this example in action](https://smilack.github.io/purescript-gesso/examples/keyboard/dist/)
