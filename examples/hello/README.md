# Hello Example

Hello is the most minimal example of a Gesso application. All imports except `Maybe`, `Effect`, and Prelude functions are fully qualified so you can start to get a feel for where each type comes from and how they fit together.

## Output

Because this example does not use the `Scaler` functions (introduced in other examples), your output may look different depending on the size of your browser window.

![Hello example output](output.png)

## Explanation

`main` for a standalone Gesso application looks similar to a simple Halogen application. 

```purescript
main :: Effect Unit
main =
  Gesso.runGessoAff do
    body <- Gesso.awaitBody
    Gesso.run Gesso.canvas canvasInput body
```