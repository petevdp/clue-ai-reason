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
  type t = StringMap.t(item);
  type normalOutcome =
    | Held(index)
    | Unheld;

  type finalOutcome =
    | Win
    | Lose;

  type outcome =
    | Normal(normalOutcome)
    | Final(finalOutcome);

  let items = t => {
    t |> StringMap.bindings |> List.map(((_, v)) => v);
  };
};

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
    let guessItemSet =
      guess
      |> StringMap.bindings
      |> List.map(((_, item): (string, item)) => item)
      |> ItemSet.of_list;

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

  let getShuffledItemCombinations = (categories: array(t)): array(Guess.t) => {
    let map =
      Array.fold_left(
        (map, c: t) =>
          StringMap.add(c.name, ItemSet.elements(c.items), map),
        StringMap.empty,
        categories,
      );

    let numCombinations: int =
      StringMap.fold(
        (_, itemList, minLength) => {
          let len = List.length(itemList);
          minLength > len ? len : minLength;
        },
        map,
        max_int,
      );

    let shuffledTruncated =
      StringMap.map(
        (itemList: list(item)) => {
          itemList
          |> Array.of_list
          |> Belt.Array.slice(~offset=0, ~len=numCombinations)
          |> Belt.Array.shuffle
        },
        map,
      );

    let combinations = ref([]);

    for (i in 0 to numCombinations - 1) {
      let guess =
        StringMap.fold(
          (name, itemArr, guess) => {
            let item = itemArr[i];
            StringMap.add(name, item, guess);
          },
          shuffledTruncated,
          StringMap.empty,
        );

      combinations := [guess, ...combinations^];
    };

    Array.of_list(combinations^);
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