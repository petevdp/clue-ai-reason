open GlobalTypes;
open Utils;

type item = string;

module ItemSet = {
  include Set.Make(String);
  let randomItem = t => {
    t |> elements |> Belt.List.shuffle |> List.hd;
  };
};

module Guess = {
  type t = array(item);
  type normalOutcome =
    | Held(index)
    | Unheld;

  type finalOutcome =
    | Win
    | Lose;

  type outcome =
    | Normal(normalOutcome)
    | Final(finalOutcome);

  let compare = (a: t, b: t) => {
    let currCompare = ref(0);
    let index = ref(0);
    while (index^ < Array.length(a) && currCompare^ == 0) {
      currCompare := String.compare(a[index^], b[index^]);
    };

    currCompare^;
  };
};

module GuessSet = Set.Make(Guess);

module Player = {
  type t = {
    name: string,
    lost: bool,
    items: ItemSet.t,
  };

  let nextPlayerIndex = (players: array(t), currentIndex: index) => {
    let offset = ref(1);
    let out = ref(-1);

    while (out^ == (-1) || offset^ < Array.length(players)) {
      let index = (currentIndex + offset^) mod Array.length(players);
      let player = players[index];
      if (!player.lost) {
        out := index;
      };
      offset := offset^ + 1;
    };

    out^;
  };

  let activePlayerIndexes = (players: array(t)) => {
    players
    |> Array.mapi((i, p) => (i, p))
    |> Array.to_list
    |> List.filter(((_, p: t)) => !p.lost)
    |> List.map(((i, _)) => i);
  };

  type action =
    | NormalAction(Guess.t)
    | FinalAction(Guess.t);
};

module Turn = {
  type normalTurn = {
    currentPlayerIndex: index,
    guess: Guess.t,
    players: array(Player.t),
    outcome: Guess.normalOutcome,
  };

  type finalTurn = {
    currentPlayerIndex: index,
    guess: Guess.t,
    players: array(Player.t),
    outcome: Guess.finalOutcome,
  };

  type t =
    | NormalTurn(normalTurn)
    | FinalTurn(finalTurn);

  let isGameEndingTurn = t => {
    switch (t) {
    | NormalTurn(_) => false
    | FinalTurn(turn) =>
      let playersAlive =
        turn.players
        |> Array.to_list
        |> List.filter((player: Player.t) => !player.lost)
        |> List.length;

      let gameOverIfLose = playersAlive == 2;
      switch (turn.outcome, gameOverIfLose) {
      | (Win, _)
      | (Lose, true) => true
      | (Lose, false) => false
      };
    };
  };
  let performFinalTurn =
      (answer: Guess.t, history: list(t), guess): finalTurn => {
    let (players, lastTurnPlayerIndex) =
      switch (List.hd(history)) {
      | NormalTurn({players, currentPlayerIndex}) => (
          players,
          currentPlayerIndex,
        )
      | FinalTurn({players, currentPlayerIndex}) => (
          players,
          currentPlayerIndex,
        )
      };
    let outcome = guess == answer ? Guess.Win : Guess.Lose;
    let currentPlayerIndex =
      Player.nextPlayerIndex(players, lastTurnPlayerIndex);
    {outcome, players, guess, currentPlayerIndex};
  };

  let performNormalTurn = (history: list(t), guess: Guess.t): normalTurn => {
    let (players, lastTurnPlayerIndex) =
      switch (List.hd(history)) {
      | NormalTurn({players, currentPlayerIndex}) => (
          players,
          currentPlayerIndex,
        )
      | FinalTurn({players, currentPlayerIndex}) => (
          players,
          currentPlayerIndex,
        )
      };
    let guessItemSet = guess |> Array.to_list |> ItemSet.of_list;
    let playerHoldingItem =
      players
      |> Array.to_list
      |> List.mapi((i, p) => (i, p))
      |> List.find_opt(((_, p: Player.t)) => {
           let heldItems = p.items |> ItemSet.inter(guessItemSet);
           ItemSet.cardinal(heldItems) > 0;
         });

    let outcome =
      switch (playerHoldingItem) {
      | Some((index, _)) => Guess.Held(index)
      | None => Unheld
      };
    let currentPlayerIndex =
      Player.nextPlayerIndex(players, lastTurnPlayerIndex);

    {outcome, players, guess, currentPlayerIndex};
  };
};

module Category = {
  type t = {
    name: string,
    items: ItemSet.t,
  };

  let getShuffledItemCombinations = (categories: array(t)) => {
    categories
    |> Array.map((a: t) =>
         a.items |> ItemSet.elements |> Belt.List.shuffle |> Array.of_list
       )
    |> Utils.unzip2dArray;
  };

  let allItems = (categories: array(t)) => {
    Array.fold_left(
      (set, category) => ItemSet.union(set, category.items),
      ItemSet.empty,
      categories,
    );
  };
};

let startingPlayers = [|"alice", "bob", "carol", "dan"|];

let startingCategories: array(Category.t) = [|
  {
    name: "locations",
    items:
      ItemSet.of_list([
        "kitchen",
        "living room",
        "shed",
        "driveway",
        "master bedroom",
        "bathroom",
      ]),
  },
  {
    name: "weapons",
    items: ItemSet.of_list(["hammer", "chainsaw", "butcher knife", "pencil"]),
  },
  {
    name: "suspects",
    items:
      ItemSet.of_list(["dirty harry", "cool hand luke", "ted cruz", "OJ"]),
  },
|];