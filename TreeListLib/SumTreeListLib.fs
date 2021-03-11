module Sunlighter.SumTreeListLib

type SumTreeList<'a, 's> =
  | Empty
  | Leaf of 'a * 's
  | TwoNode of SumTwoNodeData<'a, 's>
  | ThreeNode of SumThreeNodeData<'a, 's>
and SumTwoNodeData<'a, 's> =
  { N2_First : 'a ;
    N2_Length : int ;
    N2_Height : int ;
    N2_Left : SumTreeList<'a, 's> ;
    N2_Right : SumTreeList<'a, 's> ;
    N2_Sum : 's ;
  }
and SumThreeNodeData<'a, 's> =
  { N3_First : 'a ;
    N3_Length : int ;
    N3_Height : int ;
    N3_Left : SumTreeList<'a, 's> ;
    N3_Middle : SumTreeList<'a, 's> ;
    N3_Right : SumTreeList<'a, 's> ;
    N3_Sum : 's ;
  }

type SumInsertResult<'a, 's> =
  | IR_One of SumTreeList<'a, 's>
  | IR_Two of SumTreeList<'a, 's> * SumTreeList<'a, 's>

type SumDeleteResult<'a, 's> =
  | DR_One of SumTreeList<'a, 's>
  | DR_Zero of SumTreeList<'a, 's>

type ISumTraits<'a, 's> =
  interface
    abstract member Zero : 's
    abstract member Count : 'a -> 's
    abstract member Add : 's * 's -> 's
  end

module SumTreeList =

  [<GeneralizableValue>]
  let empty = Empty

  let height (t : SumTreeList<'a, 's>) =
    match t with
      | Empty -> -1
      | Leaf _ -> 0
      | TwoNode nd -> nd.N2_Height
      | ThreeNode nd -> nd.N3_Height

  let length (t : SumTreeList<'a, 's>) =
    match t with
      | Empty -> 0
      | Leaf _ -> 1
      | TwoNode nd -> nd.N2_Length
      | ThreeNode nd -> nd.N3_Length

  let isEmpty (t : SumTreeList<'a, 's>) =
    match t with
      | Empty -> true
      | _ -> false

  let first (t : SumTreeList<'a, 's>) =
    match t with
      | Empty -> failwith "Cannot find first of an empty tree"
      | Leaf (a, _) -> a
      | TwoNode nd -> nd.N2_First
      | ThreeNode nd -> nd.N3_First

  let private mkLeaf (traits : ISumTraits<'a, 's>) (item : 'a) =
    Leaf (item, traits.Count(item))

  let singleton = mkLeaf

  let sum (traits : ISumTraits<'a, 's>) (t : SumTreeList<'a, 's>) =
    match t with
      | Empty -> traits.Zero
      | Leaf (_, s) -> s
      | TwoNode nd -> nd.N2_Sum
      | ThreeNode nd -> nd.N3_Sum

  let underLimit (traits : ISumTraits<'a, 's>) (lessThan : 's -> 's -> bool) (limit : 's) (t : SumTreeList<'a, 's>) =
    let tryBreak (t : SumTreeList<'a, 's>) =
      match t with
        | Empty -> None
        | Leaf (a, _) -> None
        | TwoNode nd -> Some [ nd.N2_Left ; nd.N2_Right ]
        | ThreeNode nd -> Some [ nd.N3_Left ; nd.N3_Middle ; nd.N3_Right ]
    let tryFit (itemsSoFar : int) (sumSoFar : 's) (t : SumTreeList<'a, 's>) =
      let newSum = traits.Add(sumSoFar, (sum traits t))
      if (not (lessThan limit newSum)) then
        Some (itemsSoFar + (length t), newSum)
      else
        None
    let rec ul (itemsSoFar : int) (sumSoFar : 's) (tlist : SumTreeList<'a, 's> list) =
      match tlist with
        | [] -> itemsSoFar
        | h :: t ->
           match tryFit itemsSoFar sumSoFar h with
             | None ->
                 match tryBreak h with
                   | None ->
                       itemsSoFar
                   | Some parts ->
                       ul itemsSoFar sumSoFar parts
             | Some (newItems, newSum) ->
                       ul newItems newSum t
    ul 0 traits.Zero [ t ]

  let partialSum (traits : ISumTraits<'a, 's>) (items : int) (t : SumTreeList<'a, 's>) =
    let tryBreak (t : SumTreeList<'a, 's>) =
      match t with
        | Empty -> None
        | Leaf (a, _) -> None
        | TwoNode nd -> Some [ nd.N2_Left ; nd.N2_Right ]
        | ThreeNode nd -> Some [ nd.N3_Left ; nd.N3_Middle ; nd.N3_Right ]
    let tryFit (itemsSoFar : int) (sumSoFar : 's) (t : SumTreeList<'a, 's>) =
      let newItems = itemsSoFar + (length t)
      if newItems <= items then
        let newSum = traits.Add(sumSoFar, (sum traits t))
        Some (newItems, newSum)
      else
        None
    let rec ps (itemsSoFar : int) (sumSoFar : 's) (tlist : SumTreeList<'a, 's> list) =
      match tlist with
        | [] -> sumSoFar
        | h :: t ->
           match tryFit itemsSoFar sumSoFar h with
             | None ->
                 match tryBreak h with
                   | None ->
                       sumSoFar
                   | Some parts ->
                       ps itemsSoFar sumSoFar parts
             | Some (newItems, newSum) ->
                       ps newItems newSum t
    ps 0 traits.Zero [ t ]

  let private mk2node (traits : ISumTraits<'a, 's>) (l : SumTreeList<'a, 's>) (r : SumTreeList<'a, 's>) =
    if (height l) <> (height r) then
      failwithf "mk2node: tree heights of %i and %i are not equal" (height l) (height r)
    if (isEmpty l) then
      failwith "mk2node: can't make a two-node from empty trees"
    TwoNode
      { N2_First = first l ;
        N2_Length = (length l) + (length r) ;
        N2_Height = (height l) + 1 ;
        N2_Left = l ;
        N2_Right = r ;
        N2_Sum = traits.Add((sum traits l), (sum traits r))
      }

  let private mk3node (traits : ISumTraits<'a, 's>) (l : SumTreeList<'a, 's>) (m : SumTreeList<'a, 's>) (r : SumTreeList<'a, 's>) =
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
        N3_Right = r ;
        N3_Sum = traits.Add(traits.Add((sum traits l), (sum traits m)), (sum traits r))
      }

  let pair (traits : ISumTraits<'a, 's>) (item1 : 'a) (item2 : 'a) = mk2node traits (mkLeaf traits item1) (mkLeaf traits item2)

  let rec nth (i : int) (t : SumTreeList<'a, 's>) =
      match t with
        | Empty ->
            raise (new System.IndexOutOfRangeException())
        | Leaf (a, _) ->
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

  let last (t : SumTreeList<'a, 's>) =
    let len = length t
    if len = 0 then
      raise (new System.InvalidOperationException("Cannot find last of an empty tree"))
    else
      nth (len - 1) t

  let rec replaceNth (traits : ISumTraits<'a, 's>) (i : int) (v : 'a) (t : SumTreeList<'a, 's>) =
      match t with
        | Empty ->
            raise (new System.IndexOutOfRangeException())
        | Leaf (a, _) ->
            if i = 0 then
              mkLeaf traits v
            else
              raise (new System.IndexOutOfRangeException())
        | TwoNode nd ->
            let lcount = (length nd.N2_Left)
            if i < lcount then
              mk2node traits (replaceNth traits i v nd.N2_Left) nd.N2_Right
            else
              mk2node traits nd.N2_Left (replaceNth traits (i - lcount) v nd.N2_Right)
        | ThreeNode nd ->
            let lcount = (length nd.N3_Left)
            if i < lcount then
              mk3node traits (replaceNth traits i v nd.N3_Left) nd.N3_Middle nd.N3_Right
            else
              let i = i - lcount
              let mcount = (length nd.N3_Middle)
              if i < mcount then
                mk3node traits nd.N3_Left (replaceNth traits i v nd.N3_Middle) nd.N3_Right
              else
                mk3node traits nd.N3_Left nd.N3_Middle (replaceNth traits (i - mcount) v nd.N3_Right)

  let rec private insert_i (traits : ISumTraits<'a, 's>) (i : int) (v : 'a) (t : SumTreeList<'a, 's>) =
    match t with
      | Empty ->
          if i = 0 then
            IR_One (mkLeaf traits v)
          else
            raise (new System.IndexOutOfRangeException())
      | (Leaf (w, _) as ws) ->
          if i = 0 then
            IR_Two ((mkLeaf traits v), ws)
          elif i = 1 then
            IR_Two (ws, (mkLeaf traits v))
          else
            raise (new System.IndexOutOfRangeException())
      | TwoNode nd ->
          let lcount = (length nd.N2_Left)
          if i < lcount then
            match (insert_i traits i v nd.N2_Left) with
              | IR_One r -> IR_One (mk2node traits r nd.N2_Right)
              | IR_Two (r1, r2) -> IR_One (mk3node traits r1 r2 nd.N2_Right)
          else
            match (insert_i traits (i - lcount) v nd.N2_Right) with
              | IR_One r -> IR_One (mk2node traits nd.N2_Left r)
              | IR_Two (r1, r2) -> IR_One (mk3node traits nd.N2_Left r1 r2)
      | ThreeNode nd ->
          let lcount = (length nd.N3_Left)
          if i < lcount then
            match (insert_i traits i v nd.N3_Left) with
              | IR_One r -> IR_One (mk3node traits r nd.N3_Middle nd.N3_Right)
              | IR_Two (r1, r2) -> IR_Two ((mk2node traits r1 r2), (mk2node traits nd.N3_Middle nd.N3_Right))
          else
            let i = i - lcount
            let mcount = (length nd.N3_Middle)
            if i < mcount then
              match (insert_i traits i v nd.N3_Middle) with
                | IR_One r -> IR_One (mk3node traits nd.N3_Left r nd.N3_Right)
                | IR_Two (r1, r2) -> IR_Two ((mk2node traits nd.N3_Left r1), (mk2node traits r2 nd.N3_Right))
            else
              match (insert_i traits (i - mcount) v nd.N3_Right) with
                | IR_One r -> IR_One (mk3node traits nd.N3_Left nd.N3_Middle r)
                | IR_Two (r1, r2) -> IR_Two ((mk2node traits nd.N3_Left nd.N3_Middle), (mk2node traits r1 r2))

  let insertNth (traits : ISumTraits<'a, 's>) (i : int) (v : 'a) (t : SumTreeList<'a, 's>) =
    match (insert_i traits i v t) with
      | IR_One r -> r
      | IR_Two (r1, r2) -> mk2node traits r1 r2

  let push (traits : ISumTraits<'a, 's>) (v : 'a) (t : SumTreeList<'a, 's>) = insertNth traits (length t) v t

  let unshift (traits : ISumTraits<'a, 's>) (v : 'a) (t : SumTreeList<'a, 's>) = insertNth traits 0 v t

  let rec private concat_i (traits : ISumTraits<'a, 's>) (t1 : SumTreeList<'a, 's>) (t2 : SumTreeList<'a, 's>) =
    if (isEmpty t1) then
      IR_One t2
    elif (isEmpty t2) then
      IR_One t1
    elif (height t1) > (height t2) then
      match t1 with
        | Empty -> failwith "Wasn't expecting that"
        | Leaf _ -> failwith "Wasn't expecting that"
        | TwoNode nd ->
            match concat_i traits nd.N2_Right t2 with
              | IR_One r -> IR_One (mk2node traits nd.N2_Left r)
              | IR_Two (r1, r2) -> IR_One (mk3node traits nd.N2_Left r1 r2)
        | ThreeNode nd ->
            match concat_i traits nd.N3_Right t2 with
              | IR_One r -> IR_One (mk3node traits nd.N3_Left nd.N3_Middle r)
              | IR_Two (r1, r2) -> IR_Two ((mk2node traits nd.N3_Left nd.N3_Middle), (mk2node traits r1 r2))
    elif (height t1) < (height t2) then
      match t2 with
        | Empty -> failwith "Wasn't expecting that"
        | Leaf _ -> failwith "Wasn't expecting that"
        | TwoNode nd ->
            match concat_i traits t1 nd.N2_Left with
              | IR_One r -> IR_One (mk2node traits r nd.N2_Right)
              | IR_Two (r1, r2) -> IR_One (mk3node traits r1 r2 nd.N2_Right)
        | ThreeNode nd ->
            match concat_i traits t1 nd.N3_Left with
              | IR_One r -> IR_One (mk3node traits r nd.N3_Middle nd.N3_Right)
              | IR_Two (r1, r2) -> IR_Two ((mk2node traits r1 r2), (mk2node traits nd.N3_Middle nd.N3_Right))
    else
      assert ((height t1) = (height t2))
      IR_Two (t1, t2)

  let concat (traits : ISumTraits<'a, 's>) (t1 : SumTreeList<'a, 's>) (t2 : SumTreeList<'a, 's>) =
    match concat_i traits t1 t2 with
      | IR_One r -> r
      | IR_Two (r1, r2) -> (mk2node traits r1 r2)

  let concatList (traits : ISumTraits<'a, 's>) (tlist : SumTreeList<'a, 's> list) =
    let rec loop (sofar : SumTreeList<'a, 's>) (remain : SumTreeList<'a, 's> list) =
      match remain with
        | [] -> sofar
        | h :: t -> loop (concat traits sofar h) t
    loop empty tlist

  let private stealFromLeft (traits : ISumTraits<'a, 's>) (l : SumTreeList<'a, 's>) (r : SumDeleteResult<'a, 's>) =
    match r with
      | DR_One a -> IR_Two (l, a)
      | DR_Zero a ->
          match l with
            | Empty -> failwith "Error"
            | Leaf (b, _) ->
                assert isEmpty a
                IR_One l
            | TwoNode nd ->
                IR_One (mk3node traits nd.N2_Left nd.N2_Right a)
            | ThreeNode nd ->
                IR_Two ((mk2node traits nd.N3_Left nd.N3_Middle), (mk2node traits nd.N3_Right a))

  let private stealFromRight (traits : ISumTraits<'a, 's>) (l : SumDeleteResult<'a, 's>) (r : SumTreeList<'a, 's>) =
    match l with
      | DR_One a -> IR_Two (a, r)
      | DR_Zero a ->
          match r with
            | Empty -> failwith "Error"
            | Leaf (b, _) -> 
                assert isEmpty a
                IR_One r
            | TwoNode nd ->
                IR_One (mk3node traits a nd.N2_Left nd.N2_Right)
            | ThreeNode nd ->
                IR_Two ((mk2node traits a nd.N3_Left), (mk2node traits nd.N3_Middle nd.N3_Right))

  let rec private delete_i (traits : ISumTraits<'a, 's>) (i : int) (t : SumTreeList<'a, 's>) =
    match t with
      | Empty -> raise (new System.IndexOutOfRangeException())
      | Leaf (a, _) ->
         if i = 0 then
           DR_Zero Empty
         else
           raise (new System.IndexOutOfRangeException())
      | TwoNode nd ->
         let lcount = (length nd.N2_Left)
         if i < lcount then
           match stealFromRight traits (delete_i traits i nd.N2_Left) nd.N2_Right with
             | IR_One b -> DR_Zero b
             | IR_Two (a, b) -> DR_One (mk2node traits a b)
         else
           match stealFromLeft traits nd.N2_Left (delete_i traits (i - lcount) nd.N2_Right) with
             | IR_One b -> DR_Zero b
             | IR_Two (a, b) -> DR_One (mk2node traits a b)
      | ThreeNode nd ->
         let lcount = (length nd.N3_Left)
         if i < lcount then
           match stealFromRight traits (delete_i traits i nd.N3_Left) nd.N3_Middle with
             | IR_One b -> DR_One (mk2node traits b nd.N3_Right)
             | IR_Two (a, b) -> DR_One (mk3node traits a b nd.N3_Right)
         else
           let i = i - lcount
           let mcount = (length nd.N3_Middle)
           if i < mcount then
             match stealFromRight traits (delete_i traits i nd.N3_Middle) nd.N3_Right with
               | IR_One b -> DR_One (mk2node traits nd.N3_Left b)
               | IR_Two (a, b) -> DR_One (mk3node traits nd.N3_Left a b)
           else
             match stealFromLeft traits nd.N3_Middle (delete_i traits (i - mcount) nd.N3_Right) with
               | IR_One b -> DR_One (mk2node traits nd.N3_Left b)
               | IR_Two (a, b) -> DR_One (mk3node traits nd.N3_Left a b)

  let deleteNth (traits : ISumTraits<'a, 's>) (i : int) (t : SumTreeList<'a, 's>) =
    match delete_i traits i t with
      | DR_One a -> a
      | DR_Zero a -> a

  let pop (traits : ISumTraits<'a, 's>) (t : SumTreeList<'a, 's>) =
    let len = length t
    if len = 0 then
      raise (new System.InvalidOperationException("Cannot pop an empty tree"))
    else
      deleteNth traits (len - 1) t

  let shift (traits : ISumTraits<'a, 's>) (t : SumTreeList<'a, 's>) =
    let len = length t
    if len = 0 then
      raise (new System.InvalidOperationException("Cannot shift an empty TreeList"))
    else
      deleteNth traits 0 t

  let fold (func : 's -> 'a -> 's) (state : 's) (tree : SumTreeList<'a, 's2>) =
    let rec loop (result : 's) (todo : SumTreeList<'a, 's2> list) =
      match todo with
        | [] -> result
        | h :: t ->
            match h with
              | Empty -> loop result t
              | Leaf (a, _) -> loop (func result a) t
              | TwoNode nd -> loop result (nd.N2_Left :: nd.N2_Right :: t)
              | ThreeNode nd -> loop result (nd.N3_Left :: nd.N3_Middle :: nd.N3_Right :: t)
    loop state [ tree ]

  let foldBack (func : 'a -> 's -> 's) (state : 's) (tree : SumTreeList<'a, 's2>) =
    let rec loop (result : 's) (todo : SumTreeList<'a, 's2> list) =
      match todo with
        | [] -> result
        | h :: t ->
            match h with
              | Empty -> loop result t
              | Leaf (a, _) -> loop (func a result) t
              | TwoNode nd -> loop result (nd.N2_Right :: nd.N2_Left :: t)
              | ThreeNode nd -> loop result (nd.N3_Right :: nd.N3_Middle :: nd.N3_Left :: t)
    loop state [ tree ]

  let toList (tree : SumTreeList<'a, 's>) = foldBack (fun i lst -> i :: lst) [] tree

  let ofList (traits : ISumTraits<'a, 's>) (lst : 'a list) =
    let rec loop (remain : 'a list) (tree : SumTreeList<'a, 's>) =
      match remain with
        | [] -> tree
        | head :: tail ->
            loop tail (insertNth traits (length tree) head tree)
    loop lst empty

  let toSeq (tree : SumTreeList<'a, 's>) =
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
                | Leaf (a, _) ->
                    yield a
                    todo := t
                | TwoNode nd ->
                    todo := nd.N2_Left :: nd.N2_Right :: t
                | ThreeNode nt ->
                    todo := nt.N3_Left :: nt.N3_Middle :: nt.N3_Right :: t
    }

  let ofSeq (traits : ISumTraits<'a, 's>) (s : seq<'a>) =
    let result = ref empty
    for i in s do
      result := !result |> push traits i
    !result

  let map (traits : ISumTraits<'a2, 's2>) (func : 'a -> 'a2) (tree : SumTreeList<'a, 's>) =
    let rec loop (tree : SumTreeList<'a, 's>) =
      match tree with
        | Empty -> Empty
        | Leaf (a, _) -> mkLeaf traits (func a)
        | TwoNode nd -> mk2node traits (loop nd.N2_Left) (loop nd.N2_Right)
        | ThreeNode nd -> mk3node traits (loop nd.N3_Left) (loop nd.N3_Middle) (loop nd.N3_Right)
    loop tree

  let mapi (traits : ISumTraits<'a2, 's2>) (func : int -> 'a -> 'a2) (tree : SumTreeList<'a, 's>) =
    let rec loop (i : int) (tree : SumTreeList<'a, 's>) =
      match tree with
        | Empty -> Empty
        | Leaf (a, _) -> mkLeaf traits (func i a)
        | TwoNode nd -> mk2node traits (loop i nd.N2_Left) (loop (i + (length nd.N2_Left)) nd.N2_Right)
        | ThreeNode nd ->
            let nl = (loop i nd.N3_Left)
            let j = i + (length nd.N3_Left)
            let nm = (loop j nd.N3_Middle)
            let k = j + (length nd.N3_Middle)
            let nr = (loop k nd.N3_Right)
            (mk3node traits nl nm nr)
    loop 0 tree

  let iter (func : 'a -> unit) (tree : SumTreeList<'a, 's>) =
    let rec loop (tree : SumTreeList<'a, 's>) =
      match tree with
        | Empty -> ()
        | Leaf (a, _) -> (func a)
        | TwoNode nd -> (loop nd.N2_Left) ; (loop nd.N2_Right)
        | ThreeNode nd -> (loop nd.N3_Left) ; (loop nd.N3_Middle) ; (loop nd.N3_Right)
    loop tree

  let iteri (func : int -> 'a -> unit) (tree : SumTreeList<'a, 's>) =
    let rec loop (i : int) (tree : SumTreeList<'a, 's>) =
      match tree with
        | Empty -> ()
        | Leaf (a, _) -> (func i a)
        | TwoNode nd -> (loop i nd.N2_Left) ; (loop (i + (length nd.N2_Left)) nd.N2_Right)
        | ThreeNode nd ->
            (loop i nd.N3_Left) ;
            let j = i + (length nd.N3_Left)
            (loop j nd.N3_Middle) ;
            let k = j + (length nd.N3_Middle)
            (loop k nd.N3_Right)
    loop 0 tree

  let init (traits : ISumTraits<'a, 's>) (len : int) (func : int -> 'a) =
    let rec init_i (off : int) (len : int) =
      match len with
        | 0 -> Empty
        | 1 -> mkLeaf traits (func off)
        | 2 -> mk2node traits (mkLeaf traits (func off)) (mkLeaf traits (func (off + 1)))
        | 3 -> mk3node traits (mkLeaf traits (func off)) (mkLeaf traits (func (off + 1))) (mkLeaf traits (func (off + 2)))
        | _ ->
            let l = len / 2
            let r = len - l
            concat traits (init_i off l) (init_i (off + l) r)
    if len < 0 then
      raise (new System.ArgumentException("Length cannot be negative"))
    else
      init_i 0 len

  let toArray (tree : SumTreeList<'a, 's>) =
    let arr = Array.zeroCreate (length tree)
    iteri (fun i a -> arr.[i] <- a) tree
    arr

  let ofArray (traits : ISumTraits<'a, 's>) (arr : 'a[]) =
    init traits arr.Length (fun i -> arr.[i])

  let rev (traits : ISumTraits<'a, 's>) (tree : SumTreeList<'a, 's>) =
    let rec revi (tree : SumTreeList<'a, 's>) =
      match tree with
        | Empty -> Empty
        | (Leaf _ as lf) -> lf
        | TwoNode nd -> mk2node traits (revi nd.N2_Right) (revi nd.N2_Left)
        | ThreeNode nd -> mk3node traits (revi nd.N3_Right) (revi nd.N3_Middle) (revi nd.N3_Left)
    revi tree

  let skip (traits : ISumTraits<'a, 's>) (i : int) (tree : SumTreeList<'a, 's>) =
    let rec skip_i (i : int) (tree : SumTreeList<'a, 's>) =
      match tree with
        | Empty -> Empty
        | Leaf _ -> if i >= 1 then Empty else tree
        | TwoNode nd ->
            let lcount = (length nd.N2_Left)
            if i < lcount then
              concat traits (skip_i i nd.N2_Left) nd.N2_Right
            elif i = lcount then
              nd.N2_Right
            else
              skip_i (i - lcount) nd.N2_Right
        | ThreeNode nd ->
            let lcount = (length nd.N3_Left)
            if i < lcount then
              concat traits (concat traits (skip_i i (nd.N3_Left)) nd.N3_Middle) nd.N3_Right
            elif i = lcount then
              (mk2node traits nd.N3_Middle nd.N3_Right)
            else
              let i = i - lcount
              let mcount = (length nd.N3_Middle)
              if i < mcount then
                concat traits (skip_i i nd.N3_Middle) nd.N3_Right
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

  let take (traits : ISumTraits<'a, 's>) (i : int) (tree : SumTreeList<'a, 's>) =
    let rec take_i (i : int) (tree : SumTreeList<'a, 's>) =
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
              concat traits nd.N2_Left (take_i (i - lcount) nd.N2_Right)
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
                concat traits nd.N3_Left (take_i i nd.N3_Middle)
              elif i = mcount then
                mk2node traits nd.N3_Left nd.N3_Middle
              else
                concat traits nd.N3_Left (concat traits nd.N3_Middle (take_i (i - mcount) nd.N3_Right))
    if i <= 0 then
      Empty
    elif i >= (length tree) then
      tree
    else
      take_i i tree
