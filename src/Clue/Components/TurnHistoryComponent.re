open Utils;
module TurnComponent = {
  open Clue.Turn;
  [@react.component]
  let make = (~turn, ~index) => {
    let {playerIndex, guess, players, outcome} = turn;
    let player = players[playerIndex];
    let outcomeMessage =
      switch (outcome) {
      | Normal(Held(index)) => "Held by " ++ players[index].name
      | Normal(Unheld) => "Unheld"
      | Final(Win) => "Won game!"
      | Final(Lose) => "Lost game"
      };

    <tr>
      <th scope="row"> {index->string_of_int->React.string} </th>
      <td> {React.string(player.name)} </td>
      <td> <GuessComponent guess /> </td>
      <td> {React.string(outcomeMessage)} </td>
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
        <th scope="col"> {React.string("Guess")} </th>
        <th scope="col"> {React.string("Outcome")} </th>
      </tr>
    </thead>
    <tbody> bodyContent </tbody>
  </table>;
};