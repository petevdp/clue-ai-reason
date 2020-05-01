open ComponentUtils;
open Utils;
module TurnComponent = {
  open Clue.Turn;
  [@react.component]
  let make = (~turn, ~index) => {
    let {playerIndex, players, turnAction} = turn;
    let player = players[playerIndex];
    let normalAccusationMessage = "Normal Accusation";
    let finalAccusationMessage = "Final Accusation";
    let (actionMessage, outcomeMessage) =
      switch (turnAction) {
      | Accusation({outcome: Final(Win), accusation: {guess}}) => (
          <span>
            {str(finalAccusationMessage)}
            <GuessComponent guess />
          </span>,
          str("Won game!"),
        )
      | Accusation({outcome: Final(Lose), accusation: {guess}}) => (
          <span>
            {str(finalAccusationMessage)}
            <GuessComponent guess />
          </span>,
          str("Lost game!"),
        )
      | Accusation({
          outcome: Normal(Held(heldPlayerIndex)),
          accusation: {guess},
        }) => (
          <span>
            {str(normalAccusationMessage)}
            <GuessComponent guess />
          </span>,
          str("Held by " ++ players[heldPlayerIndex].name),
        )
      | Accusation({outcome: Normal(Unheld), accusation: {guess}}) => (
          <span>
            {str(normalAccusationMessage)}
            <GuessComponent guess />
          </span>,
          str("Unheld"),
        )
      | ShowHidden => (str("Show Hidden"), <span />)
      | NoAction => (str("No Action"), <span />)
      };

    let rowStyle = Clue.Player.isControlled(player) ? "table-info" : "";

    <tr className=rowStyle>
      <th scope="row"> {index->string_of_int->React.string} </th>
      <td> {React.string(player.name)} </td>
      <td> actionMessage </td>
      <td> outcomeMessage </td>
    </tr>;
  };
};

[@react.component]
let make = (~history: list(Clue.Turn.t)) => {
  let turnRowElements =
    List.mapi(
      (index, turn: Clue.Turn.t) => {
        let index = List.revIndex(index, history);
        let key = string_of_int(index);
        <TurnComponent key turn index />;
      },
      history,
    )
    |> Array.of_list;

  let bodyContent =
    Array.length(turnRowElements) > 0
      ? React.array(turnRowElements)
      : <tr> <td> {React.string("No turns made yet!")} </td> </tr>;

  <table className="table">
    <thead>
      <tr>
        <th scope="col"> {React.string("#")} </th>
        <th scope="col"> {React.string("Player")} </th>
        <th scope="col"> {React.string("Action")} </th>
        <th scope="col"> {React.string("Outcome")} </th>
      </tr>
    </thead>
    <tbody> bodyContent </tbody>
  </table>;
};