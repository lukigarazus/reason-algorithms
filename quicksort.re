let rec quicksort = (arr: array('a)): array('a) =>
  switch (Array.length(arr) == 0) {
  | true => [||]
  | _ =>
    let pivot = Array.length(arr) - 1 |> Array.get(arr);
    let lows = quicksort(arr |> arrayUtils.filter_array(~f=v => v < pivot));
    let highs = quicksort(arr |> arrayUtils.filter_array(~f=v => v > pivot));

    Array.concat([lows, [|pivot|], highs]);
  };