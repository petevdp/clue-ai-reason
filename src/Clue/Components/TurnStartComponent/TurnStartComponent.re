open ComponentUtils;
open Clue.Turn;
[@react.component]
let make = (~dispatch) => {
  <div>
    <h2> {str("Turn Start")} </h2>
    <button
      className="btn btn-primary"
      onClick={_ => dispatch(ClueReducer.ChooseTurnAction(AccusationChoice))}>
      {str("Make Accusation")}
    </button>
    <button
      className="btn btn-primary"
      onClick={_ => dispatch(ClueReducer.ChooseTurnAction(ShowHiddenChoice))}>
      {str("Show Hidden")}
    </button>
    <button
      className="btn btn-primary"
      onClick={_ => dispatch(ClueReducer.ChooseTurnAction(NoActionChoice))}>
      {str("No Action")}
    </button>
  </div>;
};