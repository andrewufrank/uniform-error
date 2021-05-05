`Error.hs` defines an uniform approach to error:
    any function which can fail returns a value of 
    - `ErrIO`, which is a wrapper around `IO`, returning a `Text` messages if the computation fails 
    - `ErrOrVal` (which is `Either Text`) for functions not in the IO monad

All cases where functions use other error signaling methods must be converted as part of their integration into the uniform framework. 

Especially important are the functions to convert to regular IO: 

- `callIO` which calls an IO function and catches a possible error return.  

- converts back functions running in `ErrIO a` into (normal) `IO (ErrOrVal a)` which is used when functions from the uniform style are used where functions in the normal IO monad are expected. 

Some other functions are helpful to identify hard to track problems in Haskell: 

`undef` to find where an undefined value is used, producing an error message. 

The StartApp is a bridge between the ErrIO used here and the standard IO monad in `main:: IO ()`

The package depends on uniform-string.
