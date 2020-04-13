open Utils;

type state = Clue.state;

type formChange =
  | ItemValue((string, string))
  | ToggleFinal;

type action =
  | SubmitTurn
  | TurnFormChange(formChange);

let initialize =
    (categories: array(Clue.Category.t), playerNames: array(string)): state => {
  let shuffledItemCombinations =
    Clue.Category.getShuffledItemCombinations(categories);

  // let truncatedItems =
  let answer = shuffledItemCombinations[0];

  let otherItems: array(Clue.item) =
    Belt.Array.sliceToEnd(shuffledItemCombinations, 1)
    |> Array.fold_left(
         (acc, guess) => {
           let itemArr = guess |> Clue.Guess.items |> Array.of_list;
           Array.append(acc, itemArr);
         },
         [||],
       );

  let hidden =
    otherItems
    |> Belt.Array.slice(~offset=0, ~len=2)
    |> Array.to_list
    |> Clue.ItemSet.of_list;

  let startingPlayers: array(Clue.Player.t) =
    playerNames
    |> Array.map((name) =>
         ({name, lost: false, items: Clue.ItemSet.empty}: Clue.Player.t)
       );

  let playerItems = Belt.Array.sliceToEnd(otherItems, 2);

  Belt.Array.forEachWithIndex(
    playerItems,
    (i, item) => {
      let playerIndex = i mod Array.length(startingPlayers);
      let player = startingPlayers[playerIndex];

      startingPlayers[playerIndex] = {
        ...player,
        items: Clue.ItemSet.add(item, player.items),
      };
    },
  );

  let turnForm = Clue.Turn.getEmptyForm(categories);

  {answer, startingPlayers, categories, hidden, history: [], turnForm};
};

exception NotImplemented;

let playerActionReducer =
    (prevState: state, playerAction: Clue.Player.action): state => {
  let {answer, history, startingPlayers}: state = prevState;
  let turn =
    Clue.Turn.performTurn(answer, history, startingPlayers, playerAction);
  {...prevState, history: [turn, ...prevState.history]};
};

let turnFormChangeReducer = (prevState: state, change: formChange): state => {
  let {turnForm}: state = prevState;

  let turnForm =
    switch (change) {
    | ItemValue((key, value)) => {
        ...turnForm,
        values: StringMap.add(key, Some(value), turnForm.values),
      }
    | ToggleFinal => {...turnForm, final: !turnForm.final}
    };

  {...prevState, turnForm};
};

let turnFormSubmitReducer = (prevState: state): state => {
  let {turnForm, answer, history, startingPlayers}: state = prevState;
  let guess = Clue.Turn.getGuessFromForm(turnForm);

  let turn =
    turnForm.final
      ? Clue.Turn.performFinalTurn(answer, history, startingPlayers, guess)
      : Clue.Turn.performNormalTurn(history, startingPlayers, guess);

  let turnForm = Clue.Turn.getEmptyForm(prevState.categories);

  {...prevState, turnForm, history: [turn, ...history]};
};

let reducer = (prevState: state, action: action): state => {
  let phase = Clue.State.determinePhase(prevState);
  switch (phase, action) {
  | (Playing(_), SubmitTurn) => turnFormSubmitReducer(prevState)
  | (Playing(_), TurnFormChange(change)) =>
    turnFormChangeReducer(prevState, change)
  | (Ended(_), _) => prevState
  };
};