[@react.component]
let make = (~category: Clue.Category.t) => {
  <div> {React.string("category: " ++ category.name)} </div>;
};