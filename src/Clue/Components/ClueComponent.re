open ComponentUtils;
module PhaseAlertComponent = {
  [@react.component]
  let make = (~phase, ~players: array(Clue.Player.t)) => {
    let (className, message) =
      switch (phase) {
      | Clue.State.Playing(index) => (
          "alert alert-primary",
          "Turn: " ++ players[index].name,
        )
      | Clue.State.Ended(index) => (
          "alert alert-success",
          "Game Over! " ++ players[index].name ++ " is the winner!",
        )
      };
    <div className> {R.string(message)} </div>;
  };
};

[@react.component]
let make =
    (~categories: array(Clue.Category.t), ~playerNames: array(string)) => {
  let (state, dispatch) =
    React.useReducer(
      ClueReducer.reducer,
      ClueReducer.initialize(categories, playerNames),
    );

  let {answer, hidden, history, turnForm}: ClueReducer.state = state;

  let hiddenItemElements =
    hidden
    |> Clue.ItemSet.to_array
    |> Array.map(item => <li key=item> {R.string(item)} </li>);

  let phase = Clue.State.determinePhase(state);
  let players = Clue.State.currentPlayers(state);

  <article className="container">
    <PhaseAlertComponent phase players />
    <GuessFormComponent dispatch categories turnForm />
    <TurnHistoryComponent history />
    <PlayersComponent players />
    <div>
      <h4> {R.string("Answer: ")} </h4>
      {<GuessComponent guess=answer />}
    </div>
    <div>
      <h4> {R.string("Hidden")} </h4>
      <ul> {R.array(hiddenItemElements)} </ul>
    </div>
  </article>;
};