open Utils;
open ComponentUtils;
module Style = {
  open Css;

  let buttonList = style([display(`flex), flexDirection(column)]);

  let button = selected => {
    let base = "list-group-item list-group-item-action";
    selected ? base ++ " active" : base;
  };
};

type category = Clue.Category.t;

[@react.component]
let make = (~category, ~selectedItem, ~dispatch) => {
  let {itemNames}: category = category;
  let itemElements =
    itemNames
    |> StringSet.to_array
    |> Array.map(itemName => {
         let item = Clue.Category.itemfromCategoryAndName(category, itemName);
         let onClick = e => {
           ReactEvent.Mouse.preventDefault(e);
           Clue.Accusation.(
             dispatch(ClueReducer.AccusationFormChange(ItemValue(item)))
           );
         };

         let isSelected =
           switch (selectedItem) {
           | None => false
           | Some(value) => value == itemName
           };

         <a key=itemName onClick className={Style.button(isSelected)}>
           {R.string(itemName)}
         </a>;
       });

  <div className="col-md-4">
    <h5> {R.string(category.name)} </h5>
    <div className="list-group"> {R.array(itemElements)} </div>
  </div>;
};