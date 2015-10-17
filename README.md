# TreeListLib
This is an F# immutable list data structure based on balanced trees.

This data structure can be used wherever the `ImmutableList<T>` from Microsoft's [immutable collections](http://www.nuget.org/packages/System.Collections.Immutable) can be used, but the interface of this library follows F# idioms more closely.

Since F# already offers immutable sets and maps, it might be easier to drop this into your project than to take a dependency on the immutable collections. (I really like using Microsoft's immutable collections in C#, actually...)

This implementation is based on 2-3 trees which are a precursor of red-black trees. I learned about 2-3 trees from a book by Robert Sedgewick, and I wrote prototypes of this code in Scheme and OCaml before porting it to F#.

Operations such as insertion, replacement, and deletion occur in logarithmic time.
