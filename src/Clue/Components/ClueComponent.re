[@react.component]
let make =
    (~categories: array(Clue.Category.t), ~playerNames: array(string)) => {
  let (state, dispatch) =
    React.useReducer(
      ClueReducer.reducer,
      ClueReducer.initialize(categories, playerNames),
    );
  let categoryElements =
    Array.map(c => <CategoryComponent category=c />, categories);

  <div> <ul> {ReasonReact.array(categoryElements)} </ul> </div>;
};