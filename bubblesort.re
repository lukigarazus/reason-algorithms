let rec bubble_sort = arr => {
  let swaps = ref(0);
  let len = Array.length(arr);
  for (n in 0 to len - 1) {
    n + 1 >= len ?
      0 :
      arr[n] <= arr[n + 1] ?
        0 :
        {
          let old = arr[n];
          arr[n] = arr[n + 1];
          arr[n + 1] = old;
          swaps := swaps^ + 1;
          0;
        };
  };
  switch swaps^ {
  | 0 => arr;
  | _ => bubble_sort(arr);
  };
};