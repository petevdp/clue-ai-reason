let unzip2dArray = (arr: array(array('a))): array(array('a)) => {
  let minInnerLength =
    arr
    |> Array.map(s => Array.length(s))
    |> Array.fold_left((min, b) => min > b ? b : min, max_int);

  let len = Array.length(arr);

  let out = Array.init(minInnerLength, _ => []);

  for (i in 0 to minInnerLength - 1) {
    for (j in 0 to len - 1) {
      let currInner = arr[j];
      let value = currInner[i];
      out[i] = [value, ...out[i]];
    };
  };

  Array.map(list => list |> Array.of_list |> Belt.Array.reverse, out);
};

module StringMap = {
  include Map.Make(String);

  let values = t => {
    t |> bindings |> List.map(((_, v)) => v);
  };

  let keys = t => {
    t |> bindings |> List.map(((k, _)) => k);
  };
};

module Array = {
  include Array;
  let join = (separator: string, arr: array(string)) => {
    Array.fold_left(
      (acc, elt) =>
        switch (acc) {
        | "" => elt
        | str => str ++ separator ++ elt
        },
      "",
      arr,
    );
  };
};

module List = {
  include List;

  let revIndex = (index, t) => List.length(t) - index;
};

module Set = (Type: Set.OrderedType) => {
  include Set.Make(Type);
  let randomItem = t => {
    t |> elements |> Belt.List.shuffle |> List.hd;
  };

  let to_array = t => t |> elements |> Array.of_list;
};

module StringSet = Set(String);

exception NotFound;
exception NotImplemented;