open ComponentUtils;
module GamePhaseAlertComponent = {
  [@react.component]
  let make = (~gamePhase, ~players: array(Clue.Player.t)) => {
    let (className, message) =
      switch (gamePhase) {
      | Clue.State.Playing(index) =>
        let player = players[index];
        let alertClass =
          Clue.Player.isControlled(player)
            ? "alert alert-info" : "alert alert-primary";
        (alertClass, "Turn: " ++ players[index].name);
      | Clue.State.Ended(index) =>
        let player = players[index];
        let alertClass =
          Clue.Player.isControlled(player)
            ? "alert alert-success" : "alert alert-error";
        (alertClass, "Game Over! " ++ player.name ++ " is the winner!");
      };
    <div className> {R.string(message)} </div>;
  };
};

[@react.component]
let make =
    (
      ~categories: array(Clue.Category.t),
      ~specifiedPlayers: array(Clue.Player.specifiedPlayer),
      ~engine: Clue.Engine.t,
      ~numHiddenItems,
    ) => {
  let (state, dispatch) =
    React.useReducer(
      ClueReducer.reducer,
      Clue.State.initialize(categories, specifiedPlayers, numHiddenItems),
    );
  Clue.Engine.useEngine(engine, state, ((guess, final)) =>
    dispatch(
      ClueReducer.EngineSubmitAccusation({
        guess,
        final,
        playerIndex: state.controlledPlayerIndex,
      }),
    )
  );

  let {history, turnPhase}: ClueReducer.state = state;

  let gamePhase = Clue.State.determineGamePhase(state);
  let players = Clue.State.currentPlayers(state);
  let currentPlayerIndex = Clue.Turn.currentPlayerIndex(history);

  let formElement =
    switch (turnPhase) {
    | Some(Start) => <TurnStartComponent dispatch />
    | Some(PendingAccusation(accusationForm)) =>
      <AccusationFormComponent
        dispatch
        categories
        accusationForm
        currentPlayerIndex
      />
    | Some(PendingShowHidden(itemSelection)) =>
      <HiddenItemsSubmissionComponent dispatch itemSelection categories />
    | Some(PendingShowHiddenUncontrolled) =>
      <div>
        <div>
          {str("Click to continue when you've shown the players your cards")}
          <button
            onClick={_ => dispatch(ChooseTurnAction(ShowHiddenChoice))}>
            {str("Show Hidden")}
          </button>
        </div>
      </div>
    | Some(PendingTurnOutcome(accusation)) =>
      <TurnOutcomeFormComponent accusation players dispatch />
    | None =>
      <button onClick={_ => dispatch(NewGame)}>
        {R.string("New Game")}
      </button>
    };

  <article className="container">
    <GamePhaseAlertComponent gamePhase players />
    <ControlPanelComponent dispatch history />
    formElement
    <TurnHistoryComponent history />
    <PlayersComponent players />
  </article>;
};