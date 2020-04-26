open ComponentUtils;
open Clue.Item;
open Utils;

type fuzzySelectionItem = {
  id: int,
  name: string,
};

[@react.component]
let make =
    (~dispatch, ~itemSelection: array(option(Clue.item)), ~categories) => {
  let allItems = Clue.Category.allItems(categories);

  let itemsToSelectFrom =
    allItems
    // make sure the items haven't been selected already
    |> Clue.ItemSet.filter(item => {
         Array.for_all(
           sItem =>
             switch (sItem) {
             | Some(sItem) => sItem != item
             | None => true
             },
           itemSelection,
         )
       })
    |> Clue.ItemSet.to_array
    |> Array.mapi((i, {name}: Clue.Item.t) => {id: i, name});

  let keys = [|"name"|];

  let resultsTemplate: FuzzySearch.resultsTemplate(fuzzySelectionItem) =
    (_, state, styles, _) => {
      state.results
      |> Array.mapi((i, item: fuzzySelectionItem) => {
           let style =
             state.selectedIndex == i
               ? styles.selectedResultStyle : styles.resultsStyle;
           <div
             key={string_of_int(i)}
             style
             onClick={_ => {
               dispatch(ClueReducer.SelectHiddenItem(item.name))
             }}>
             {str(item.name)}
           </div>;
         })
      |> R.array;
    };
  let selected: array(React.element) =
    itemSelection
    |> Array.to_list
    |> List.filter(selection =>
         switch (selection) {
         | Some(_) => true
         | None => false
         }
       )
    |> List.map(selection =>
         switch (selection) {
         | Some(item) => item
         | None => raise(Impossible)
         }
       )
    |> Array.of_list
    |> Array.map((item: Clue.Item.t) => {
         let {name, itemType}: Clue.Item.t = item;
         let typeMessage = str(Clue.Item.typeAsString(itemType));
         let nameMessage = str(name);
         <div className="list-group-item" key=name>
           <span className="item-type"> typeMessage </span>
           {str(" - ")}
           <span className="item-name"> nameMessage </span>
         </div>;
       });

  let selectionsLeft =
    itemSelection
    |> Array.to_list
    |> List.filter(item =>
         switch (item) {
         | Some(_) => false
         | None => true
         }
       )
    |> List.length;

  let selectedElement =
    Array.length(selected) > 0
      ? R.array(selected) : str("No items selected");

  let searchElement =
    selectionsLeft > 0
      ? <FuzzySearch
          list=itemsToSelectFrom
          keys
          width=200
          onSelect={s => dispatch(ClueReducer.SelectHiddenItem(s.name))}
          resultsTemplate
        />
      : R.null;

  <div>
    searchElement
    {str(" - ")}
    <div className="list-group"> selectedElement </div>
    {str(" - ")}
    {selectionsLeft |> string_of_int |> str}
    {str(" left")}
  </div>;
};