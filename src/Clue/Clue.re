open GlobalTypes;
open Utils;

type item = string;

module ItemSet = {
  include Set.Make(String);
  let randomItem = t => {
    t |> elements |> Belt.List.shuffle |> List.hd;
  };

  let to_array = t => t |> elements |> Array.of_list;
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

module Category = {
  type t = {
    name: string,
    items: ItemSet.t,
  };

  let getByName = (name: string, arr: array(t)) => {
    switch (Belt.Array.getBy(arr, c => c.name == name)) {
    | Some(category) => category
    | None => raise(NotFound)
    };
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

  let to_map = t => {
    Array.fold_left(
      (map, {name, items}) => {StringMap.add(name, items, map)},
      StringMap.empty,
      t,
    );
  };

  let allItems = (categories: array(t)) => {
    Array.fold_left(
      (set, category) => ItemSet.union(set, category.items),
      ItemSet.empty,
      categories,
    );
  };
};

module Turn = {
  type t = {
    playerIndex: index,
    guess: Guess.t,
    players: array(Player.t),
    outcome: Guess.outcome,
  };

  type form = {
    values: StringMap.t(option(item)),
    final: bool,
  };

  let winning = (t: t) =>
    switch (t.outcome) {
    | Guess.Final(Guess.Win) => true
    | _ => false
    };

  let getEmptyForm = categories => {
    values: categories |> Category.to_map |> StringMap.map(_ => None),
    final: false,
  };

  let canFormBeSubmitted = ({values}) => {
    StringMap.values(values) |> List.for_all(v => v != None);
  };

  exception InvalidGuessForm;

  let getGuessFromForm = ({values}) => {
    StringMap.map(
      value =>
        switch (value) {
        | Some(item) => item
        | None => raise(InvalidGuessForm)
        },
      values,
    );
  };

  let last = (history: list(t)) => Belt.List.head(history);

  let currentPlayers = (history, startingPlayers) =>
    switch (last(history)) {
    | None => startingPlayers
    | Some({players}) => players
    };

  exception NoPlayersLeft;

  let currentPlayerIndex = history => {
    switch (last(history)) {
    | None => 0
    | Some({playerIndex, players}) =>
      let activeIndexes = Player.activePlayerIndexes(players);
      switch (activeIndexes) {
      | [] => raise(NoPlayersLeft)
      | activeIndexes =>
        let index = ref((playerIndex + 1) mod Array.length(players));
        while (!List.mem(index^, activeIndexes)) {
          index := (index^ + 1) mod Array.length(players);
        };

        index^;
      };
    };
  };

  let isGameEndingTurn = t => {
    switch (t.outcome) {
    | Normal(_) => false
    | Final(outcome) =>
      let playersAlive =
        t.players
        |> Array.to_list
        |> List.filter((player: Player.t) => !player.lost)
        |> List.length;

      let gameOverIfLose = playersAlive == 2;
      switch (outcome, gameOverIfLose) {
      | (Win, _)
      | (Lose, true) => true
      | (Lose, false) => false
      };
    };
  };

  let performFinalTurn =
      (
        answer: Guess.t,
        history: list(t),
        startingPlayers: array(Player.t),
        guess: Guess.t,
      )
      : t => {
    let players = currentPlayers(history, startingPlayers);
    let outcome =
      Guess.Final(
        StringMap.values(guess) == StringMap.values(answer)
          ? Guess.Win : Guess.Lose,
      );
    {outcome, players, guess, playerIndex: currentPlayerIndex(history)};
  };

  let performNormalTurn =
      (history: list(t), startingPlayers: array(Player.t), guess: Guess.t)
      : t => {
    let players = currentPlayers(history, startingPlayers);

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
      | Some((index, _)) => Guess.Normal(Guess.Held(index))
      | None => Guess.Normal(Unheld)
      };

    {outcome, players, guess, playerIndex: currentPlayerIndex(history)};
  };

  let performTurn =
      (
        answer: Guess.t,
        history: list(t),
        startingPlayers: array(Player.t),
        playerAction: Player.action,
      )
      : t => {
    switch (playerAction) {
    | Player.NormalAction(guess) =>
      performNormalTurn(history, startingPlayers, guess)
    | Player.FinalAction(guess) =>
      performFinalTurn(answer, history, startingPlayers, guess)
    };
  };
};

module State = {
  type t = {
    history: list(Turn.t),
    startingPlayers: array(Player.t),
    answer: Guess.t,
    categories: array(Category.t),
    hidden: ItemSet.t,
    turnForm: Turn.form,
  };

  type phase =
    | Playing(index)
    | Ended(index);

  let currentPlayers = t => Turn.currentPlayers(t.history, t.startingPlayers);

  exception NotEnoughPlayers;
  let determinePhase = t => {
    let players = currentPlayers(t);
    let lastTurn = Turn.last(t.history);
    let currentPlayerIndex = Turn.currentPlayerIndex(t.history);
    switch (Player.activePlayerIndexes(players), lastTurn) {
    | ([], _) => raise(NotEnoughPlayers)
    | ([lastPlayerIndex], _) => Ended(lastPlayerIndex)
    | (_, Some(lastTurn)) =>
      Turn.winning(lastTurn)
        ? Ended(lastTurn.playerIndex) : Playing(currentPlayerIndex)
    | (_, None) => Playing(currentPlayerIndex)
    };
  };
};

type state = State.t;

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
        // "master bedroom",
        // "bathroom",
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