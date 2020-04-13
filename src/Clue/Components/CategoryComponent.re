[@react.component]
let make = (~category: Clue.Category.t) => {
  let itemElements =
    category.items
    |> Clue.ItemSet.to_array
    |> Array.map(item => <li key=item> {React.string(item)} </li>);

  <li>
    <h4> {React.string(category.name)} </h4>
    <ul> {React.array(itemElements)} </ul>
  </li>;
};