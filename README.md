# reactive-dom

Definitions in this project help the programmer to wire up a reactive DOM user
interface, expressed in Haskell via reactive-sequence and ghcjs-dom, and
compiled to JavaScript using ghcjs.

This is **experimental** software, but it's also **working** software!
That's subject to one restriction: there is an
[issue](https://github.com/ghcjs/shims/pull/25#issuecomment-154876738)
with weak references in ghcjs, but it is resolved in
[this branch](https://github.com/alios/shims/tree/fix-reactive-banana).
