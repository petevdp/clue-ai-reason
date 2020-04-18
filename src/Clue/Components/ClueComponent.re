open ComponentUtils;
module GamePhaseAlertComponent = {
  [@react.component]
  let make = (~gamePhase, ~players: array(Clue.Player.t)) => {
    let (className, message) =
      switch (gamePhase) {
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
    (
      ~categories: array(Clue.Category.t),
      ~specifiedPlayers: array(Clue.Player.specifiedPlayer),
    ) => {
  let (state, dispatch) =
    React.useReducer(
      ClueReducer.reducer,
      Clue.State.initialize(categories, specifiedPlayers),
    );

  let {history, turnPhase}: ClueReducer.state = state;

  let gamePhase = Clue.State.determineGamePhase(state);
  let players = Clue.State.currentPlayers(state);
  let currentPlayerIndex = Clue.Turn.currentPlayerIndex(history);

  let formElement =
    switch (turnPhase) {
    | Some(PendingAccusation(accusationForm)) =>
      <AccusationFormComponent
        dispatch
        categories
        accusationForm
        currentPlayerIndex
      />
    | Some(PendingTurnOutcome(accusation)) =>
      <TurnOutcomeFormComponent accusation players dispatch />
    | None =>
      <button onClick={_ => dispatch(NewGame)}>
        {R.string("New Game")}
      </button>
    };

  <article className="container">
    <GamePhaseAlertComponent gamePhase players />
    formElement
    <TurnHistoryComponent history />
    <PlayersComponent players />
  </article>;
};