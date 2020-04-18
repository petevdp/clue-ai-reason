open Utils;

[@react.component]
let make = (~category: Clue.Category.t) => {
  let itemElements =
    category.itemNames
    |> StringSet.to_array
    |> Array.map(itemName => <li key=itemName> {React.string(itemName)} </li>);

  <li>
    <h4> {React.string(category.name)} </h4>
    <ul> {React.array(itemElements)} </ul>
  </li>;
};