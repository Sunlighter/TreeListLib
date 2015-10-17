module TreeListLib

/// <summary>
/// An immutable indexed list.
/// </summary>
type TreeList<'a>

module TreeList = begin
  /// <summary>
  /// An empty TreeList.
  /// </summary>
  [<GeneralizableValueAttribute ()>]
  val empty : TreeList<'a>

  /// <summary>
  /// Returns the number of items in the given TreeList.
  /// </summary>
  val length : t:TreeList<'a> -> int

  /// <summary>
  /// Returns true if the given TreeList is empty and false otherwise.
  /// </summary>
  val isEmpty : t:TreeList<'a> -> bool

  /// <summary>
  /// Returns the first item in a TreeList. Error if the TreeList is empty.
  /// </summary>
  val first : t:TreeList<'a> -> 'a

  /// <summary>
  /// Returns a TreeList containing only the given single item.
  /// </summary>
  val singleton : item:'a -> TreeList<'a>

  /// <summary>
  /// Returns a TreeList containing only the given two items, in order.
  /// </summary>
  val pair : item1:'a -> item2:'a -> TreeList<'a>

  /// <summary>
  /// Returns the nth item in a TreeList. Logarithmic time.
  /// </summary>
  val nth : i:int -> t:TreeList<'a> -> 'a

  /// <summary>
  /// Returns the last item in a TreeList.
  /// </summary>
  val last : t:TreeList<'a> -> 'a

  /// <summary>
  /// Replaces the nth item in the given TreeList with the given item.
  /// </summary>
  val replaceNth : i:int -> v:'a -> t:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Inserts the given item so that it becomes nth. The item that was nth becomes (n+1)th and so forth.
  /// </summary>
  val insertNth : i:int -> v:'a -> t:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Appends the given item to the end of the TreeList.
  /// </summary>
  val push : v:'a -> t:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Appends the given item to the beginning of the TreeList, shifting subsequent items to the next higher index.
  /// </summary>
  val unshift : v:'a -> t:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Concatenates the given TreeLists.
  /// </summary>
  val concat : t1:TreeList<'a> -> t2:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Concatenates all the TreeLists in the list to form a single TreeList.
  /// </summary>
  val concatList : tlist:TreeList<'a> list -> TreeList<'a>

  /// <summary>
  /// Removes the nth item from the given TreeList, shifting subsequent items to the next lower index.
  /// </summary>
  val deleteNth : i:int -> t:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Removes the last item from the given TreeList.
  /// </summary>
  val pop : t:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Removes the first item from the given TreeList, shifting subsequent items to the next lower index.
  /// </summary>
  val shift : t:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Uses each item in the given TreeList, from left to right, to update a state using the given function.
  /// </summary>
  val fold : func : ('s -> 'a -> 's) -> state : 's -> tree : TreeList<'a> -> 's

  /// <summary>
  /// Uses each item in the given TreeList, from right to left, to update a state using the given function.
  /// </summary>
  val foldBack : func : ('a -> 's -> 's) -> state : 's -> tree : TreeList<'a> -> 's

  /// <summary>
  /// Converts a TreeList to a regular list.
  /// </summary>
  val toList : tree:TreeList<'a> -> 'a list

  /// <summary>
  /// Converts a regular list to a TreeList.
  /// </summary>
  val ofList : lst:'a list -> TreeList<'a>

  /// <summary>
  /// Converts a TreeList to a sequence of its items.
  /// </summary>
  val toSeq : tree:TreeList<'a> -> 'a seq

  /// <summary>
  /// Converts a sequence of items to a TreeList.
  /// </summary>
  val ofSeq : 'a seq -> TreeList<'a>

  /// <summary>
  /// Constructs a new TreeList by applying a function to every item in the given TreeList.
  /// </summary>
  val map : func:('a -> 'b) -> tree:TreeList<'a> -> TreeList<'b>

  /// <summary>
  /// Constructs a new TreeList by applying a function to every item and its index in the given TreeList.
  /// </summary>
  val mapi : func:(int -> 'a -> 'b) -> tree:TreeList<'a> -> TreeList<'b>

  /// <summary>
  /// Passes every item in the given TreeList to a visitor function.
  /// </summary>
  val iter : func:('a -> unit) -> tree:TreeList<'a> -> unit

  /// <summary>
  /// Passes every item in the given TreeList, and its index, to a visitor function.
  /// </summary>
  val iteri : func:(int -> 'a -> unit) -> tree:TreeList<'a> -> unit

  /// <summary>
  /// Creates a TreeList of a given length from the items returend by the given generator function.
  /// </summary>
  val init : len:int -> func:(int -> 'a) -> TreeList<'a>

  /// <summary>
  /// Converts a TreeList to an array.
  /// </summary>
  val toArray : tree:TreeList<'a> -> 'a[]

  /// <summary>
  /// Converts an array to a TreeList.
  /// </summary>
  val ofArray : arr:'a[] -> TreeList<'a>

  /// <summary>
  /// Reverses the given TreeList.
  /// </summary>
  val rev : tree:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Removes the given number of items from the beginning of the TreeList.
  /// </summary>
  val skip : i:int -> tree:TreeList<'a> -> TreeList<'a>

  /// <summary>
  /// Returns a TreeList consisting only of the first N items of the given TreeList.
  /// </summary>
  val take : i:int -> tree:TreeList<'a> -> TreeList<'a>
end

/// <summary>
/// Concatenates two TreeLists.
/// </summary>
val inline (@) : t1:TreeList<'a> -> t2:TreeList<'a> -> TreeList<'a>
