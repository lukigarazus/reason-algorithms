let rec halve = arr => {
  let len = arr |> Array.length;
  let hl = len / 2;
  let h1 = Array.sub(arr, 0, hl);
  let h2 = Array.sub(arr, hl, len - hl);
  (h1, h2);
};

let rec merge = (~l1=?,~l2=?,arr1, arr2) => {
  let l1 = switch l1 {
  	| Some(v) => v
    | None => Array.length(arr1)
  }
  let l2 = switch l2 {
  	| Some(v) => v
    | None => Array.length(arr2)
  }
  let tupl = (l1, l2);
  switch (tupl) {
  | (0, 0) => arr1
  | (0, _) => arr2
  | (_, 0) => arr1
  | (l1, l2) =>
    arr1[0] > arr2[0] ?
      Array.append(
        [|arr2[0]|],
        merge(Array.sub(arr2, 1, l2 - 1), arr1),
      ) :
      Array.append(
        [|arr1[0]|],
        merge(Array.sub(arr1, 1, l1 - 1), arr2),
      )
  };
};

let rec mergesort = arr => {
  let (h1, h2) = halve(arr);
  switch (Array.length(h1), Array.length(h2)) {
  | (0,0) => h1
  | (_,0) => h1
  | (0,_) => h2
  | (_,_) => merge(mergesort(h1),mergesort(h2));
  }
};