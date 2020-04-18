open Utils;
open ComponentUtils;
open Clue.Turn;

module NormalOutcomeFormComponent = {
  module Style = {
    open Css;
    let container =
      "col-md-4 "
      ++ style([
           display(`flex),
           flexDirection(`column),
           alignItems(`center),
         ]);
  };

  [@react.component]
  let make = (~players, ~dispatch) => {
    let playerButtonElements =
      players
      |> Array.mapi((i, {name}: Clue.Player.t) => {
           let onClick = _ => {
             dispatch(ClueReducer.SubmitTurnOutcome(Normal(Held(i))));
           };
           <button className="btn btn-primary" type_="button" key=name onClick>
             {R.string(name)}
           </button>;
         });
    let unheldOnClick = _ =>
      dispatch(ClueReducer.SubmitTurnOutcome(Normal(Unheld)));
    let unheldButtonElement =
      <button
        className="btn btn-dark"
        type_="button"
        key="unheld"
        onClick=unheldOnClick>
        {R.string("Nobody holds the cards from the guess")}
      </button>;

    <>
      <h2> {R.string("Who had the card?")} </h2>
      <div className="btn-group"> {R.array(playerButtonElements)} </div>
      unheldButtonElement
    </>;
  };
};

module FinalOutcomeFormComponent = {
  [@react.component]
  let make = (~dispatch) => {
    let onClickWin = _ => {
      dispatch(ClueReducer.SubmitTurnOutcome(Final(Win)));
    };
    let onClickLose = _ => {
      dispatch(ClueReducer.SubmitTurnOutcome(Final(Lose)));
    };
    <div className="btn-group">
      {R.string("final outcome!")}
      <button className="btn btn-primary" onClick=onClickWin>
        {R.string("Win")}
      </button>
      <button className="btn btn-primary" onClick=onClickLose>
        {R.string("Lose")}
      </button>
    </div>;
  };
};

[@react.component]
let make = (~accusation: Clue.Accusation.t, ~players, ~dispatch) => {
  let formElement =
    accusation.final
      ? <FinalOutcomeFormComponent dispatch />
      : <NormalOutcomeFormComponent players dispatch />;

  <section className="card"> formElement </section>;
};