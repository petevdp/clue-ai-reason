type state = {
  history: list(Clue.Turn.t),
  startingPlayers: array(Clue.Player.t),
  answer: Clue.Guess.t,
  categories: array(Clue.Category.t),
  hidden: Clue.ItemSet.t,
};

type action =
  | PlayerAction(Clue.Player.action);
// | FinalGuess
// | Rewind(int);

type phase =
  | Playing
  | Ended;

let initialize =
    (categories: array(Clue.Category.t), playerNames: array(string)): state => {
  let shuffledItemCombinations =
    Clue.Category.getShuffledItemCombinations(categories);

  // let truncatedItems =
  let answer = shuffledItemCombinations[0];

  let otherItems: array(Clue.item) =
    Belt.Array.sliceToEnd(shuffledItemCombinations, 1)
    |> Array.fold_left(
         (acc, combination) => Array.append(acc, combination),
         [||],
       );

  let hidden =
    otherItems
    |> Belt.Array.slice(~offset=0, ~len=2)
    |> Array.to_list
    |> Clue.ItemSet.of_list;
  Js.log(hidden);

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

  {categories, history: [], startingPlayers, answer, hidden};
};

exception NotImplemented;

let determinePhase = (history): phase => {
  switch (Belt.List.head(history)) {
  | None => Playing
  | Some(turn) => Clue.Turn.isGameEndingTurn(turn) ? Ended : Playing
  };
};

let playerActionReducer =
    (prevState: state, playerAction: Clue.Player.action): state => {
  let {answer, history} = prevState;
  let turn =
    switch (playerAction) {
    | NormalAction(guess) =>
      Clue.Turn.NormalTurn(Clue.Turn.performNormalTurn(history, guess))
    | FinalAction(guess) =>
      Clue.Turn.FinalTurn(Clue.Turn.performFinalTurn(answer, history, guess))
    };

  {...prevState, history: [turn, ...prevState.history]};
};

let reducer = (prevState: state, action: action): state => {
  let phase = determinePhase(prevState.history);
  switch (phase, action) {
  | (Playing, PlayerAction(action)) =>
    playerActionReducer(prevState, action)
  | (Ended, PlayerAction(_)) => prevState
  };
};