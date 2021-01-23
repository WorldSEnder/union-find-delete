# union-find-delete

A small Haskell library that implements Union/Find with constant time
deletion by Alstrup, Gørtz, Rauhe, Thorup, Zwick. Useful, for example,
to implement unification in a type inference system.

The Union/Find algorithm implements these operations in constant-time,
or effectively constant time (*):

 1. (*) Find the descriptor of the equivalence class.

 2. (*) Check whether two elements are in the same equivalence class.

 3. (*) Create a union of two equivalence classes.

 4. Remove an element from an equivalence class
