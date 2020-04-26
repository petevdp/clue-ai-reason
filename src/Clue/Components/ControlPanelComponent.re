open ComponentUtils;

[@react.component]
let make = (~dispatch, ~history) => {
  <>
    <button
      type_="button"
      disabled={history == []}
      className="btn btn-primary"
      onClick={_ => dispatch(ClueReducer.UndoLastTurn)}>
      {R.string("Undo Last")}
    </button>
    <button
      type_="button"
      className="btn btn-primary"
      onClick={_ => dispatch(ClueReducer.NewGame)}>
      {R.string("New Game")}
    </button>
  </>;
};