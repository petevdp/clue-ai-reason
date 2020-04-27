type state = Clue.State.t;

type action =
  | NewGame
  | ChooseTurnAction(Clue.Turn.turnActionChoice)
  | SubmitAccusation
  | EngineSubmitAccusation(Clue.Accusation.t)
  | UndoLastTurn
  | ControlledPlayerInfoFormChange
  | SubmitControlledPlayerInfo
  | SelectHiddenItem(string)
  | DeleteHiddenItemSelection(string)
  | SubmitHiddenItems
  | SubmitTurnOutcome(Clue.Turn.outcome)
  | AccusationFormChange(Clue.Accusation.formChange);

exception NotImplemented;

module Helpers = {
  open Clue.State;
  open Clue.Turn;
  let isControlledPlayersTurn = ({history, controlledPlayerIndex}) => {
    currentPlayerIndex(history) == controlledPlayerIndex;
  };
};

exception UnexpectedChooseTurnActionType;
let chooseTurnActionType = (prevState, actionType) => {
  let {turnPhase, numHiddenItems}: state = prevState;
  let turnPhase =
    switch (
      turnPhase,
      actionType,
      Helpers.isControlledPlayersTurn(prevState),
    ) {
    | (Some(Start), Clue.Turn.AccusationChoice, _) =>
      Clue.Turn.PendingAccusation(Clue.Accusation.emptyForm)
    | (Some(Start), Clue.Turn.ShowHiddenChoice, true) =>
      PendingShowHidden(Array.make(numHiddenItems, None))
    | (Some(Start), Clue.Turn.ShowHiddenChoice, false) =>
      PendingShowHiddenUncontrolled
    | _ => raise(UnexpectedChooseTurnActionType)
    };

  {...prevState, turnPhase: Some(turnPhase)};
};

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

let submitEngineAccusation = (prevState, accusation: Clue.Accusation.t) => {
  exception UnexpectedEngineAccusationSubmission;
  let {turnPhase, history}: state = prevState;
  let isEnginesTurn =
    Clue.Turn.currentPlayerIndex(history) === accusation.playerIndex;
  let turnPhase =
    switch (turnPhase, isEnginesTurn) {
    | (Some(PendingAccusation(_)), true) =>
      Some(Clue.Turn.PendingTurnOutcome(accusation))
    | (_, _) => raise(UnexpectedEngineAccusationSubmission)
    };

  {...prevState, turnPhase};
};

exception UnexpectedHiddenItemSelection;
let selectHiddenItem = (prevState, name) => {
  let {turnPhase, categories}: state = prevState;

  let selectedItem =
    categories |> Clue.Category.allItems |> Clue.ItemSet.findByName(name);

  let itemSelections: array(option(Clue.item)) =
    switch (turnPhase) {
    | Some(Clue.Turn.PendingShowHidden(itemSelections)) => itemSelections
    | _ => raise(UnexpectedHiddenItemSelection)
    };

  let openIndex =
    itemSelections->Belt.Array.getIndexBy(elt =>
      switch (elt) {
      | None => true
      | Some(_) => false
      }
    );

  let updatedItemSelections =
    switch (openIndex) {
    | Some(index) =>
      let selections = Array.copy(itemSelections);
      selections[index] = Some(selectedItem);
      selections;
    | None => itemSelections
    };

  {...prevState, turnPhase: Some(PendingShowHidden(updatedItemSelections))};
};

exception InvalidHiddenItemSubmission;
exception UnexpectedHiddenItemSubmission;
let hiddenItemSubmissionReducer = prevState => {
  let {turnPhase}: state = prevState;
  let statePostReveal =
    switch (turnPhase, Helpers.isControlledPlayersTurn(prevState)) {
    | (Some(PendingShowHidden(items)), true) =>
      let items =
        Array.map(
          e =>
            switch (e) {
            | Some(item) => item
            | None => raise(InvalidHiddenItemSubmission)
            },
          items,
        )
        |> Clue.ItemSet.of_array;
      {...prevState, hiddenItems: Some(items)};
    | (Some(PendingShowHidden(_)), false) => prevState
    | (_, _) => raise(UnexpectedHiddenItemSubmission)
    };
  let {history}: state = statePostReveal;
  let playerIndex = Clue.Turn.currentPlayerIndex(history);
  let players = Clue.State.currentPlayers(statePostReveal);
  let turnAction = Clue.Turn.ShowHidden;
  let turn: Clue.Turn.t = {playerIndex, players, turnAction};
  {...statePostReveal, history: [turn, ...history], turnPhase: Some(Start)};
};

exception UnexpectedTurnOutcomeSubmission;
exception InvalidTurnOutcomeSubmission;
let turnOutcomeSubmissionReducer = (prevState: state, outcome) => {
  open Clue.Turn;
  let {history, startingPlayers}: state = prevState;
  let accusation =
    switch (prevState.turnPhase) {
    | Some(PendingTurnOutcome(accusation)) => accusation
    | _ => raise(UnexpectedTurnOutcomeSubmission)
    };

  let players = currentPlayers(history, startingPlayers);
  let playerIndex = currentPlayerIndex(history);
  let turn = {
    turnAction: {
      Accusation({accusation, outcome});
    },
    playerIndex,
    players,
  };

  let turnPhase = isGameEndingTurn(turn) ? None : startingPhase;

  {...prevState, turnPhase, history: [turn, ...history]};
};

let resetState = (prevState: state) => {
  let history = [];
  let turnPhase = Clue.Turn.startingPhase;
  let hiddenItems = None;
  {...prevState, history, turnPhase, hiddenItems};
};

exception UndoWhenFirstTurn;
let undoLastTurn = (prevState: state) => {
  let history =
    switch (prevState.history) {
    | [] => raise(UndoWhenFirstTurn)
    | [_, ...rest] => rest
    };
  let turnPhase = Clue.Turn.startingPhase;
  {...prevState, history, turnPhase};
};

exception ActionNotSupported;
let reducer = (prevState: state, action: action): state => {
  let gamePhase = Clue.State.determineGamePhase(prevState);
  let state =
    switch (gamePhase, action) {
    | (Playing(_), ChooseTurnAction(actionType)) =>
      chooseTurnActionType(prevState, actionType)
    | (Playing(_), SubmitAccusation) => submitAccusationForm(prevState)
    | (Playing(_), SelectHiddenItem(name)) =>
      selectHiddenItem(prevState, name)
    | (Playing(_), SubmitHiddenItems) =>
      hiddenItemSubmissionReducer(prevState)
    | (Playing(_), AccusationFormChange(change)) =>
      accusationFormChangeReducer(prevState, change)
    | (Playing(_), EngineSubmitAccusation(accusation)) =>
      submitEngineAccusation(prevState, accusation)
    | (Playing(_), SubmitTurnOutcome(outcome)) =>
      turnOutcomeSubmissionReducer(prevState, outcome)
    | (_, UndoLastTurn) => undoLastTurn(prevState)
    | (_, NewGame) => resetState(prevState)
    | _ => raise(ActionNotSupported)
    };

  // let {history, controlledPlayerIndex, turnPhase}: Clue.state = state;
  // let isControlledPlayersTurn = Clue.Turn.currentPlayerIndex(history) == controlledPlayerIndex

  // switch(isControlledPlayersTurn, turnPhase) {
  //   | (true, Some(Clue.Turn.PendingAccusation(_))) =>
  // };

  state;
};