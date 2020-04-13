open ComponentUtils;
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
  let actionMessage =
    switch (phase) {
    | Clue.State.Playing(index) => "Turn: " ++ players[index].name
    | Clue.State.Ended => "Ended!:"
    };

  <article className="container">
    <div> {R.string(actionMessage)} </div>
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