let rec insertsort = (~n=1, arr) =>
  switch (n == Array.length(arr)) {
  | true => arr
  | _ =>
    let b = ref(false);
    for (i in 0 to n - 1) {
      if (! b^) {
        let old = arr[n];
        switch (arr[i] > old) {
        | true =>
          Js.Array.spliceInPlace(i, 0, [|old|], arr);
          Js.Array.spliceInPlace(n + 1, 1, [||], arr);
          b := true;
          ();
        | _ => ()
        };
      };
    };
    insertsort(~n=n + 1, arr);
  };
