let filter_array = (~f, arr: array('a)): array('a) =>
  arr |> Array.to_list |> List.filter(f) |> Array.of_list;
