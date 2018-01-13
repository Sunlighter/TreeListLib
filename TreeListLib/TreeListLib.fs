module Sunlighter.TreeListLib

type TreeList<'a> =
  | Empty
  | Leaf of 'a
  | TwoNode of TwoNodeData<'a>
  | ThreeNode of ThreeNodeData<'a>
and TwoNodeData<'a> =
  { N2_First : 'a ;
    N2_Length : int ;
    N2_Height : int ;
    N2_Left : TreeList<'a> ;
    N2_Right : TreeList<'a>
  }
and ThreeNodeData<'a> =
  { N3_First : 'a ;
    N3_Length : int ;
    N3_Height : int ;
    N3_Left : TreeList<'a> ;
    N3_Middle : TreeList<'a> ;
    N3_Right : TreeList<'a>
  }

type InsertResult<'a> =
  | IR_One of TreeList<'a>
  | IR_Two of TreeList<'a> * TreeList<'a>

type DeleteResult<'a> =
  | DR_One of TreeList<'a>
  | DR_Zero of TreeList<'a>

module TreeList =

  [<GeneralizableValue>]
  let empty = Empty

  let height (t : TreeList<'a>) =
    match t with
      | Empty -> -1
      | Leaf _ -> 0
      | TwoNode nd -> nd.N2_Height
      | ThreeNode nd -> nd.N3_Height

  let length (t : TreeList<'a>) =
    match t with
      | Empty -> 0
      | Leaf _ -> 1
      | TwoNode nd -> nd.N2_Length
      | ThreeNode nd -> nd.N3_Length

  let isEmpty (t : TreeList<'a>) =
    match t with
      | Empty -> true
      | _ -> false

  let first (t : TreeList<'a>) =
    match t with
      | Empty -> failwith "Cannot find first of an empty tree"
      | Leaf a -> a
      | TwoNode nd -> nd.N2_First
      | ThreeNode nd -> nd.N3_First

  let singleton (item : 'a) = Leaf item

  let private mk2node (l : TreeList<'a>) (r : TreeList<'a>) =
    if (height l) <> (height r) then
      failwithf "mk2node: tree heights of %i and %i are not equal" (height l) (height r)
    if (isEmpty l) then
      failwith "mk2node: can't make a two-node from empty trees"
    TwoNode
      { N2_First = first l ;
        N2_Length = (length l) + (length r) ;
        N2_Height = (height l) + 1 ;
        N2_Left = l ;
        N2_Right = r
      }

  let private mk3node (l : TreeList<'a>) (m : TreeList<'a>) (r : TreeList<'a>) =
    let (a, b, c) = ((height l), (height m), (height r))
    if a <> b || b <> c then
      failwithf "mk3node: tree heights of %i, %i, and %i are not equal" a b c
    if (isEmpty l) then
      failwith "mk3node: can't make a three-node from empty trees"
    ThreeNode
      { N3_First = first l ;
        N3_Length = (length l) + (length m) + (length r) ;
        N3_Height = (height l) + 1 ;
        N3_Left = l;
        N3_Middle = m;
        N3_Right = r
      }

  let pair (item1 : 'a) (item2 : 'a) = mk2node (Leaf item1) (Leaf item2)

  let rec nth (i : int) (t : TreeList<'a>) =
    match t with
      | Empty ->
          raise (new System.IndexOutOfRangeException())
      | Leaf a ->
          if i = 0 then
            a
          else
            raise (new System.IndexOutOfRangeException())
      | TwoNode nd ->
          if i = 0 then
            nd.N2_First
          else
            let lcount = (length nd.N2_Left)
            if i < lcount then
              nth i nd.N2_Left
            else
              nth (i - lcount) nd.N2_Right
      | ThreeNode nd ->
          if i = 0 then
            nd.N3_First
          else
            let lcount = (length nd.N3_Left)
            if i < lcount then
              nth i nd.N3_Left
            else
              let i2 = i - lcount
              let mcount = (length nd.N3_Middle)
              if i2 < mcount then
                nth i2 nd.N3_Middle
              else
                nth (i2 - mcount) nd.N3_Right

  let last (t : TreeList<'a>) =
    let len = length t
    if len = 0 then
      raise (new System.InvalidOperationException("Cannot find last of an empty tree"))
    else
      nth (len - 1) t

  let rec replaceNth (i : int) (v : 'a) (t : TreeList<'a>) =
    match t with
      | Empty ->
          raise (new System.IndexOutOfRangeException())
      | Leaf a ->
          if i = 0 then
            Leaf v
          else
            raise (new System.IndexOutOfRangeException())
      | TwoNode nd ->
          let lcount = (length nd.N2_Left)
          if i < lcount then
            mk2node (replaceNth i v nd.N2_Left) nd.N2_Right
          else
            mk2node nd.N2_Left (replaceNth (i - lcount) v nd.N2_Right)
      | ThreeNode nd ->
          let lcount = (length nd.N3_Left)
          if i < lcount then
            mk3node (replaceNth i v nd.N3_Left) nd.N3_Middle nd.N3_Right
          else
            let i = i - lcount
            let mcount = (length nd.N3_Middle)
            if i < mcount then
              mk3node nd.N3_Left (replaceNth i v nd.N3_Middle) nd.N3_Right
            else
              mk3node nd.N3_Left nd.N3_Middle (replaceNth (i - mcount) v nd.N3_Right)

  let rec private insert_i (i : int) (v : 'a) (t : TreeList<'a>) =
    match t with
      | Empty ->
          if i = 0 then
            IR_One (Leaf v)
          else
            raise (new System.IndexOutOfRangeException())
      | Leaf w ->
          if i = 0 then
            IR_Two ((Leaf v), (Leaf w))
          elif i = 1 then
            IR_Two ((Leaf w), (Leaf v))
          else
            raise (new System.IndexOutOfRangeException())
      | TwoNode nd ->
          let lcount = (length nd.N2_Left)
          if i < lcount then
            match (insert_i i v nd.N2_Left) with
              | IR_One r -> IR_One (mk2node r nd.N2_Right)
              | IR_Two (r1, r2) -> IR_One (mk3node r1 r2 nd.N2_Right)
          else
            match (insert_i (i - lcount) v nd.N2_Right) with
              | IR_One r -> IR_One (mk2node nd.N2_Left r)
              | IR_Two (r1, r2) -> IR_One (mk3node nd.N2_Left r1 r2)
      | ThreeNode nd ->
          let lcount = (length nd.N3_Left)
          if i < lcount then
            match (insert_i i v nd.N3_Left) with
              | IR_One r -> IR_One (mk3node r nd.N3_Middle nd.N3_Right)
              | IR_Two (r1, r2) -> IR_Two ((mk2node r1 r2), (mk2node nd.N3_Middle nd.N3_Right))
          else
            let i = i - lcount
            let mcount = (length nd.N3_Middle)
            if i < mcount then
              match (insert_i i v nd.N3_Middle) with
                | IR_One r -> IR_One (mk3node nd.N3_Left r nd.N3_Right)
                | IR_Two (r1, r2) -> IR_Two ((mk2node nd.N3_Left r1), (mk2node r2 nd.N3_Right))
            else
              match (insert_i (i - mcount) v nd.N3_Right) with
                | IR_One r -> IR_One (mk3node nd.N3_Left nd.N3_Middle r)
                | IR_Two (r1, r2) -> IR_Two ((mk2node nd.N3_Left nd.N3_Middle), (mk2node r1 r2))

  let insertNth (i : int) (v : 'a) (t : TreeList<'a>) =
    match (insert_i i v t) with
      | IR_One r -> r
      | IR_Two (r1, r2) -> mk2node r1 r2

  let push (v : 'a) (t : TreeList<'a>) = insertNth (length t) v t

  let unshift (v : 'a) (t : TreeList<'a>) = insertNth 0 v t

  let rec private concat_i (t1 : TreeList<'a>) (t2 : TreeList<'a>) =
    if (isEmpty t1) then
      IR_One t2
    elif (isEmpty t2) then
      IR_One t1
    elif (height t1) > (height t2) then
      match t1 with
        | Empty -> failwith "Wasn't expecting that"
        | Leaf a -> failwith "Wasn't expecting that"
        | TwoNode nd ->
            match concat_i nd.N2_Right t2 with
              | IR_One r -> IR_One (mk2node nd.N2_Left r)
              | IR_Two (r1, r2) -> IR_One (mk3node nd.N2_Left r1 r2)
        | ThreeNode nd ->
            match concat_i nd.N3_Right t2 with
              | IR_One r -> IR_One (mk3node nd.N3_Left nd.N3_Middle r)
              | IR_Two (r1, r2) -> IR_Two ((mk2node nd.N3_Left nd.N3_Middle), (mk2node r1 r2))
    elif (height t1) < (height t2) then
      match t2 with
        | Empty -> failwith "Wasn't expecting that"
        | Leaf a -> failwith "Wasn't expecting that"
        | TwoNode nd ->
            match concat_i t1 nd.N2_Left with
              | IR_One r -> IR_One (mk2node r nd.N2_Right)
              | IR_Two (r1, r2) -> IR_One (mk3node r1 r2 nd.N2_Right)
        | ThreeNode nd ->
            match concat_i t1 nd.N3_Left with
              | IR_One r -> IR_One (mk3node r nd.N3_Middle nd.N3_Right)
              | IR_Two (r1, r2) -> IR_Two ((mk2node r1 r2), (mk2node nd.N3_Middle nd.N3_Right))
    else
      assert ((height t1) = (height t2))
      IR_Two (t1, t2)

  let concat (t1 : TreeList<'a>) (t2 : TreeList<'a>) =
    match concat_i t1 t2 with
      | IR_One r -> r
      | IR_Two (r1, r2) -> (mk2node r1 r2)

  let concatList (tlist : TreeList<'a> list) =
    let rec loop (sofar : TreeList<'a>) (remain : TreeList<'a> list) =
      match remain with
        | [] -> sofar
        | h :: t -> loop (concat sofar h) t
    loop empty tlist

  let private stealFromLeft (l : TreeList<'a>) (r : 'a DeleteResult) =
    match r with
      | DR_One a -> IR_Two (l, a)
      | DR_Zero a ->
          match l with
            | Empty -> failwith "Error"
            | Leaf b ->
                assert isEmpty a
                IR_One l
            | TwoNode nd ->
                IR_One (mk3node nd.N2_Left nd.N2_Right a)
            | ThreeNode nd ->
                IR_Two ((mk2node nd.N3_Left nd.N3_Middle), (mk2node nd.N3_Right a))

  let private stealFromRight (l : 'a DeleteResult) (r : TreeList<'a>) =
    match l with
      | DR_One a -> IR_Two (a, r)
      | DR_Zero a ->
          match r with
            | Empty -> failwith "Error"
            | Leaf b -> 
                assert isEmpty a
                IR_One r
            | TwoNode nd ->
                IR_One (mk3node a nd.N2_Left nd.N2_Right)
            | ThreeNode nd ->
                IR_Two ((mk2node a nd.N3_Left), (mk2node nd.N3_Middle nd.N3_Right))

  let rec private delete_i (i : int) (t : TreeList<'a>) =
    match t with
      | Empty -> raise (new System.IndexOutOfRangeException())
      | Leaf a ->
         if i = 0 then
           DR_Zero Empty
         else
           raise (new System.IndexOutOfRangeException())
      | TwoNode nd ->
         let lcount = (length nd.N2_Left)
         if i < lcount then
           match stealFromRight (delete_i i nd.N2_Left) nd.N2_Right with
             | IR_One b -> DR_Zero b
             | IR_Two (a, b) -> DR_One (mk2node a b)
         else
           match stealFromLeft nd.N2_Left (delete_i (i - lcount) nd.N2_Right) with
             | IR_One b -> DR_Zero b
             | IR_Two (a, b) -> DR_One (mk2node a b)
      | ThreeNode nd ->
         let lcount = (length nd.N3_Left)
         if i < lcount then
           match stealFromRight (delete_i i nd.N3_Left) nd.N3_Middle with
             | IR_One b -> DR_One (mk2node b nd.N3_Right)
             | IR_Two (a, b) -> DR_One (mk3node a b nd.N3_Right)
         else
           let i = i - lcount
           let mcount = (length nd.N3_Middle)
           if i < mcount then
             match stealFromRight (delete_i i nd.N3_Middle) nd.N3_Right with
               | IR_One b -> DR_One (mk2node nd.N3_Left b)
               | IR_Two (a, b) -> DR_One (mk3node nd.N3_Left a b)
           else
             match stealFromLeft nd.N3_Middle (delete_i (i - mcount) nd.N3_Right) with
               | IR_One b -> DR_One (mk2node nd.N3_Left b)
               | IR_Two (a, b) -> DR_One (mk3node nd.N3_Left a b)

  let deleteNth (i : int) (t : TreeList<'a>) =
    match delete_i i t with
      | DR_One a -> a
      | DR_Zero a -> a

  let pop (t : TreeList<'a>) =
    let len = length t
    if len = 0 then
      raise (new System.InvalidOperationException("Cannot pop an empty tree"))
    else
      deleteNth (len - 1) t

  let shift (t : TreeList<'a>) =
    let len = length t
    if len = 0 then
      raise (new System.InvalidOperationException("Cannot shift an empty TreeList"))
    else
      deleteNth 0 t

  let fold (func : 's -> 'a -> 's) (state : 's) (tree : TreeList<'a>) =
    let rec loop (result : 's) (todo : TreeList<'a> list) =
      match todo with
        | [] -> result
        | h :: t ->
            match h with
              | Empty -> loop result t
              | Leaf a -> loop (func result a) t
              | TwoNode nd -> loop result (nd.N2_Left :: nd.N2_Right :: t)
              | ThreeNode nd -> loop result (nd.N3_Left :: nd.N3_Middle :: nd.N3_Right :: t)
    loop state [ tree ]

  let foldBack (func : 'a -> 's -> 's) (state : 's) (tree : TreeList<'a>) =
    let rec loop (result : 's) (todo : TreeList<'a> list) =
      match todo with
        | [] -> result
        | h :: t ->
            match h with
              | Empty -> loop result t
              | Leaf a -> loop (func a result) t
              | TwoNode nd -> loop result (nd.N2_Right :: nd.N2_Left :: t)
              | ThreeNode nd -> loop result (nd.N3_Right :: nd.N3_Middle :: nd.N3_Left :: t)
    loop state [ tree ]

  let toList (tree : TreeList<'a>) = foldBack (fun i lst -> i :: lst) [] tree

  let ofList (lst : 'a list) =
    let rec loop (remain : 'a list) (tree : TreeList<'a>) =
      match remain with
        | [] -> tree
        | head :: tail ->
            loop tail (insertNth (length tree) head tree)
    loop lst empty

  let toSeq (tree : TreeList<'a>) =
    seq {
      let todo = ref [ tree ]
      let doneFlag = ref false
      while not !doneFlag do
        match !todo with
          | [] -> doneFlag := true
          | h :: t ->
              match h with
                | Empty ->
                    todo := t
                | Leaf a ->
                    yield a
                    todo := t
                | TwoNode nd ->
                    todo := nd.N2_Left :: nd.N2_Right :: t
                | ThreeNode nt ->
                    todo := nt.N3_Left :: nt.N3_Middle :: nt.N3_Right :: t
    }

  let ofSeq (s : seq<'a>) =
    let result = ref empty
    for i in s do
      result := !result |> push i
    !result

  let map (func : 'a -> 'b) (tree : TreeList<'a>) =
    let rec loop (tree : TreeList<'a>) =
      match tree with
        | Empty -> Empty
        | Leaf a -> Leaf (func a)
        | TwoNode nd -> mk2node (loop nd.N2_Left) (loop nd.N2_Right)
        | ThreeNode nd -> mk3node (loop nd.N3_Left) (loop nd.N3_Middle) (loop nd.N3_Right)
    loop tree

  let mapi (func : int -> 'a -> 'b) (tree : TreeList<'a>) =
    let rec loop (i : int) (tree : TreeList<'a>) =
      match tree with
        | Empty -> Empty
        | Leaf a -> Leaf (func i a)
        | TwoNode nd -> mk2node (loop i nd.N2_Left) (loop (i + (length nd.N2_Left)) nd.N2_Right)
        | ThreeNode nd ->
            let nl = (loop i nd.N3_Left)
            let j = i + (length nd.N3_Left)
            let nm = (loop j nd.N3_Middle)
            let k = j + (length nd.N3_Middle)
            let nr = (loop k nd.N3_Right)
            (mk3node nl nm nr)
    loop 0 tree

  let iter (func : 'a -> unit) (tree : TreeList<'a>) =
    let rec loop (tree : TreeList<'a>) =
      match tree with
        | Empty -> ()
        | Leaf a -> (func a)
        | TwoNode nd -> (loop nd.N2_Left) ; (loop nd.N2_Right)
        | ThreeNode nd -> (loop nd.N3_Left) ; (loop nd.N3_Middle) ; (loop nd.N3_Right)
    loop tree

  let iteri (func : int -> 'a -> unit) (tree : TreeList<'a>) =
    let rec loop (i : int) (tree : TreeList<'a>) =
      match tree with
        | Empty -> ()
        | Leaf a -> (func i a)
        | TwoNode nd -> (loop i nd.N2_Left) ; (loop (i + (length nd.N2_Left)) nd.N2_Right)
        | ThreeNode nd ->
            (loop i nd.N3_Left) ;
            let j = i + (length nd.N3_Left)
            (loop j nd.N3_Middle) ;
            let k = j + (length nd.N3_Middle)
            (loop k nd.N3_Right)
    loop 0 tree

  let init (len : int) (func : int -> 'a) =
    let rec init_i (off : int) (len : int) =
      match len with
        | 0 -> Empty
        | 1 -> Leaf (func off)
        | 2 -> mk2node (Leaf (func off)) (Leaf (func (off + 1)))
        | 3 -> mk3node (Leaf (func off)) (Leaf (func (off + 1))) (Leaf (func (off + 2)))
        | _ ->
            let l = len / 2
            let r = len - l
            concat (init_i off l) (init_i (off + l) r)
    if len < 0 then
      raise (new System.ArgumentException("Length cannot be negative"))
    else
      init_i 0 len

  let toArray (tree : TreeList<'a>) =
    let arr = Array.zeroCreate (length tree)
    iteri (fun i a -> arr.[i] <- a) tree
    arr

  let ofArray (arr : 'a[]) =
    init arr.Length (fun i -> arr.[i])

  let rec rev (tree : TreeList<'a>) =
    match tree with
      | Empty -> Empty
      | Leaf a -> Leaf a
      | TwoNode nd -> mk2node (rev nd.N2_Right) (rev nd.N2_Left)
      | ThreeNode nd -> mk3node (rev nd.N3_Right) (rev nd.N3_Middle) (rev nd.N3_Left)

  let skip (i : int) (tree : TreeList<'a>) =
    let rec skip_i (i : int) (tree : TreeList<'a>) =
      match tree with
        | Empty -> Empty
        | Leaf _ -> if i >= 1 then Empty else tree
        | TwoNode nd ->
            let lcount = (length nd.N2_Left)
            if i < lcount then
              concat (skip_i i nd.N2_Left) nd.N2_Right
            elif i = lcount then
              nd.N2_Right
            else
              skip_i (i - lcount) nd.N2_Right
        | ThreeNode nd ->
            let lcount = (length nd.N3_Left)
            if i < lcount then
              concat (concat (skip_i i (nd.N3_Left)) nd.N3_Middle) nd.N3_Right
            elif i = lcount then
              (mk2node nd.N3_Middle nd.N3_Right)
            else
              let i = i - lcount
              let mcount = (length nd.N3_Middle)
              if i < mcount then
                concat (skip_i i nd.N3_Middle) nd.N3_Right
              elif i = mcount then
                nd.N3_Right
              else
                skip_i (i - mcount) nd.N3_Right
    if i <= 0 then
      tree
    elif i >= length tree then
      Empty
    else
      skip_i i tree

  let take (i : int) (tree : TreeList<'a>) =
    let rec take_i (i : int) (tree : TreeList<'a>) =
      match tree with
        | Empty -> Empty
        | Leaf _ -> if i <= 0 then tree else Empty
        | TwoNode nd ->
            let lcount = (length nd.N2_Left)
            if i < lcount then
              take_i i nd.N2_Left
            else if i = lcount then
              nd.N2_Left
            else
              concat nd.N2_Left (take_i (i - lcount) nd.N2_Right)
        | ThreeNode nd ->
            let lcount = (length nd.N3_Left)
            if i < lcount then
              take_i i nd.N3_Left
            else if i = lcount then
              nd.N3_Left
            else
              let i = i - lcount
              let mcount = (length nd.N3_Middle)
              if i < mcount then
                concat nd.N3_Left (take_i i nd.N3_Middle)
              elif i = mcount then
                mk2node nd.N3_Left nd.N3_Middle
              else
                concat nd.N3_Left (concat nd.N3_Middle (take_i (i - mcount) nd.N3_Right))
    if i <= 0 then
      Empty
    elif i >= (length tree) then
      tree
    else
      take_i i tree

let inline (@) (t1 : TreeList<'a>) (t2 : TreeList<'a>) = TreeList.concat t1 t2
