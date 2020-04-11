let unzip2dArray = (arr: array(array('a))): array(array('a)) => {
  let minInnerLength =
    arr
    |> Array.map(s => Array.length(s))
    |> Array.fold_left((min, b) => min > b ? b : min, 0);

  let len = Array.length(arr);

  let out = Array.init(minInnerLength, _ => []);

  for (i in 0 to minInnerLength) {
    for (j in 0 to len) {
      let currInner = arr[j];
      let value = currInner[i];
      out[i] = [value, ...out[i]];
    };
  };

  Array.map(list => list |> Array.of_list |> Belt.Array.reverse, out);
};

module StringMap = Map.Make(String);