type state = Clue.state;

type action =
  | NewGame
  | SubmitAccusation
  | UndoLastTurn
  | ControlledPlayerInfoFormChange
  | SubmitControlledPlayerInfo
  | HiddenItemsCheck
  | SubmitHiddenItems
  | SubmitTurnOutcome(Clue.Turn.outcome)
  | AccusationFormChange(Clue.Accusation.formChange);

exception NotImplemented;

exception UnexepctedAccusationFormChange;
let accusationFormChangeReducer =
    (prevState: state, change: Clue.Accusation.formChange): state => {
  let {turnPhase}: state = prevState;
  let accusationForm =
    switch (turnPhase, change) {
    | (
        Some(PendingAccusation(accusationForm)),
        ItemValue({itemType: Location, name}),
      ) => {
        ...accusationForm,
        guessChoice: {
          ...accusationForm.guessChoice,
          locationChoice: Some(name),
        },
      }
    | (
        Some(PendingAccusation(accusationForm)),
        ItemValue({itemType: Weapon, name}),
      ) => {
        ...accusationForm,
        guessChoice: {
          ...accusationForm.guessChoice,
          weaponChoice: Some(name),
        },
      }
    | (
        Some(PendingAccusation(accusationForm)),
        ItemValue({itemType: Suspect, name}),
      ) => {
        ...accusationForm,
        guessChoice: {
          ...accusationForm.guessChoice,
          suspectChoice: Some(name),
        },
      }
    | (Some(PendingAccusation(accusationForm)), ToggleFinal) => {
        ...accusationForm,
        final: !accusationForm.final,
      }
    | (_, _) => raise(UnexepctedAccusationFormChange)
    };

  {...prevState, turnPhase: Some(PendingAccusation(accusationForm))};
};
exception UnexpectedAccusationSubmission;
exception InvalidAccusationSubmission;
let submitAccusationForm = (prevState: state): state => {
  let {turnPhase, history}: state = prevState;
  open Clue.Turn;
  let playerIndex = currentPlayerIndex(history);
  let accusationOutput =
    switch (turnPhase) {
    | Some(PendingAccusation(accusationForm)) =>
      Clue.Accusation.accusationFromForm(accusationForm, playerIndex)
    | _ => raise(UnexpectedAccusationSubmission)
    };
  let accusation =
    switch (accusationOutput) {
    | Some(accusation) => accusation
    | None => raise(InvalidAccusationSubmission)
    };

  {...prevState, turnPhase: Some(startingPendingOutcome(accusation))};
};

exception UnexpectedTurnOutcomeSubmission;
exception InvalidTurnOutcomeSubmission;
let turnOutcomeSubmissionReducer = (prevState: state, outcome) => {
  open Clue.Turn;
  let {history, startingPlayers}: state = prevState;
  let guess =
    switch (prevState.turnPhase) {
    | Some(PendingTurnOutcome(accusation)) => accusation.guess
    | _ => raise(UnexpectedTurnOutcomeSubmission)
    };

  let players = currentPlayers(history, startingPlayers);
  let playerIndex = currentPlayerIndex(history);
  let turn = {guess, playerIndex, players, outcome};

  let turnPhase = isGameEndingTurn(turn) ? None : startingPhase;

  {...prevState, turnPhase, history: [turn, ...history]};
};

let resetState = (prevState: state) => {
  let history = [];
  let turnPhase = Clue.Turn.startingPhase;
  {...prevState, history, turnPhase};
};

exception ActionNotSupported;
let reducer = (prevState: state, action: action): state => {
  let phase = Clue.State.determineGamePhase(prevState);
  switch (phase, action) {
  | (Playing(_), SubmitAccusation) => submitAccusationForm(prevState)
  | (Playing(_), AccusationFormChange(change)) =>
    accusationFormChangeReducer(prevState, change)
  | (Playing(_), SubmitTurnOutcome(outcome)) =>
    turnOutcomeSubmissionReducer(prevState, outcome)
  | (_, NewGame) => resetState(prevState)
  | _ => raise(ActionNotSupported)
  };
};