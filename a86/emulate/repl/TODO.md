# REPL Implementation To-Dos


## Feature Set

  * Load a86 code, start interpreter.
  * Step through interpreter.
      * Forward.
      * Backward.
      * Arbitrary.
  * Run until halt.
  * Visualize.
      * Instructions (`.text`).
      * Stack.
      * Heap.
      * Static memory (as used).
  * Write to interpreter.
      * Replace instruction.
      * Replace memory (checked).
      * Replace memory (unsafe).
  * Accept input.
      * Could just be a string used during initialization.
          * If so, make it replaceable at run-time.
  * Show output.
  * Configure what is shown at each step.
      * Instructions, memory, etc.
  * Show "call stack"?
      * Only works when `Call` and `Ret` are used, maybe?
      * Maybe support jumps? Might be hard, though.
  * Support value masks.
      * It'd be neat if these could be loaded automatically...
          * Don't want to use a special structure for this. Maybe by name?
          * E.g., `___-mask`, `___-type`, etc are standardized for this purpose.
