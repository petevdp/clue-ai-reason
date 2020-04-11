[@react.component]
let make = (~category: Clue.Category.t) => {
  let itemElements =
    category.items
    |> Clue.ItemSet.elements
    |> Array.of_list
    |> Array.map(item => {<li key=item> {React.string(item)} </li>});

  <li key={category.name}>
    <h4> {React.string(category.name)} </h4>
    <ul> {React.array(itemElements)} </ul>
  </li>;
};