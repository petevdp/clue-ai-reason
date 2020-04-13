open ComponentUtils;
module Style = {
  open Css;

  let buttonList = style([display(`flex), flexDirection(column)]);
  let unhighlightedButton = style([]);
  let highlightedButton = style([backgroundColor(green)]);

  let button = selected => {
    let base = "list-group-item list-group-item-action";
    selected ? base ++ " active" : base;
  };
};

type category = Clue.Category.t;

[@react.component]
let make = (~category, ~selectedItem, ~dispatch) => {
  let {items, name}: category = category;
  let itemElements =
    items
    |> Clue.ItemSet.to_array
    |> Array.map(item => {
         let onClick = e => {
           ReactEvent.Mouse.preventDefault(e);
           dispatch(
             ClueReducer.TurnFormChange(ClueReducer.ItemValue((name, item))),
           );
         };

         let isSelected =
           switch (selectedItem) {
           | None => false
           | Some(value) => value == item
           };

         <a key=item onClick className={Style.button(isSelected)}>
           {R.string(item)}
         </a>;
       });

  <div className="col-md-4">
    <h5> {R.string(category.name)} </h5>
    <div className="list-group"> {R.array(itemElements)} </div>
  </div>;
};