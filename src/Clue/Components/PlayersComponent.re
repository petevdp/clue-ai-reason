open Utils;
open Clue.Player;
open ComponentUtils;
module PlayerComponent = {
  [@react.component]
  let make = (~player: t, ~index) => {
    let {name, lost, items} = player;

    let itemMessage = Array.join(", ", Clue.ItemSet.to_array(items));

    <tr>
      <th scope="row"> {index |> string_of_int |> R.string} </th>
      <td> {R.string(name)} </td>
      <td> {R.string(string_of_bool(!lost))} </td>
      <td> {R.string(itemMessage)} </td>
    </tr>;
  };
};

[@react.component]
let make = (~players: array(t)) => {
  let playerRows =
    Array.mapi(
      (index, player) => <PlayerComponent key={player.name} player index />,
      players,
    );
  let bodyContents =
    Array.length(playerRows) > 0
      ? R.array(playerRows) : <tr> <td> {R.string("no players!")} </td> </tr>;

  <section>
    <h4> {R.string("Players: ")} </h4>
    <table className="table">
      <thead>
        <tr>
          <th scope="col"> {R.string("#")} </th>
          <th scope="col"> {R.string("Name")} </th>
          <th scope="col"> {R.string("Playing")} </th>
          <th scope="col"> {R.string("Items")} </th>
        </tr>
      </thead>
      <tbody> bodyContents </tbody>
    </table>
  </section>;
};