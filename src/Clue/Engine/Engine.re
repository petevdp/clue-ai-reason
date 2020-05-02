open Clue;
open Utils;

type reducedState = {
  history: list(Turn.t),
  startingPlayers: array(Player.t),
  hiddenItems: option(ItemSet.t),
  categories: array(Category.t),
  controlledPlayerIndex: int,
};
type action = Player.action;
type t = {
  name: string,
  f: reducedState => (Guess.t, bool),
};

let getReducedState = (fullState: State.t) => {
  history: fullState.history,
  startingPlayers: fullState.startingPlayers,
  categories: fullState.categories,
  hiddenItems: None,
  controlledPlayerIndex: fullState.controlledPlayerIndex,
};

let getControlledPlayerInfo = t => {
  exception ControlledPlayerIndexIsUncontrolled;
  let {history, startingPlayers} = t;
  let currentPlayers = Turn.currentPlayers(history, startingPlayers);
  switch (currentPlayers[t.controlledPlayerIndex]) {
  | {items: Player.Controlled(info)} => info
  | _ => raise(ControlledPlayerIndexIsUncontrolled)
  };
};

let controlledItems = t => {
  getControlledPlayerInfo(t);
};

let getEngineByName = (name, engines) =>
  engines |> Array.to_list |> List.filter(e => e.name == name);

let useEngine = (engine, fullState: State.t, makeMove) => {
  React.useEffect3(
    () => {
      let isAccusationPhase =
        switch (fullState.turnPhase) {
        | Some(Turn.PendingAccusation(_)) => true
        | _ => false
        };

      if (State.isControlledPlayerTurn(fullState) && isAccusationPhase) {
        let _ =
          Js.Global.setTimeout(
            () => fullState |> getReducedState |> engine.f |> makeMove,
            1,
          );
        ();
      };
      None;
    },
    (fullState, engine, makeMove),
  );
};

module Random = {
  let f = state => {
    let {categories} = state;
    let guess: Guess.t =
      Category.{
        location: categories |> getByName("locations") |> randomItem,
        weapon: categories |> getByName("weapons") |> randomItem,
        suspect: categories |> getByName("suspects") |> randomItem,
      };

    (guess, true);
  };

  let engine = {name: "random", f};
};

module V1 = {
  let getAllPossibleGuesses = categories => {
    let guesses = ref(GuessSet.empty);

    let locations = Category.get(Location, categories).itemNames;
    let weapons = Category.get(Location, categories).itemNames;
    let suspects = Category.get(Suspect, categories).itemNames;
    StringSet.(
      locations
      |> iter(location => {
           weapons
           |> iter(weapon => {
                suspects
                |> iter(suspect => {
                     let guess: Guess.t = {location, weapon, suspect};
                     guesses := GuessSet.add(guess, guesses^);
                   })
              })
         })
    );

    guesses^;
  };

  let filterBasedOnItems = (items, allGuesses) => {
    let itemNames = ItemSet.names(items);
    GuessSet.filter(
      ({location, weapon, suspect}) => {
        open StringSet;
        let itemsInGuess =
          mem(location, itemNames)
          || mem(weapon, itemNames)
          || mem(suspect, itemNames);

        !itemsInGuess;
      },
      allGuesses,
    );
  };

  let filterBasedOnHistory = (history, guesses) => {
    let accusationActions =
      history
      |> List.filter((turn: Turn.t) =>
           switch (turn.turnAction) {
           | Accusation(_) => true
           | _ => false
           }
         )
      |> List.map((turn: Turn.t) => {
           switch (turn.turnAction) {
           | Accusation(accusation) => accusation
           | _ => raise(Impossible)
           }
         });

    let guesses = ref(guesses);
    List.iter(
      ({accusation, outcome}: Turn.accusationAction) => {
        Js.log(GuessSet.cardinal(guesses^));
        let {guess}: Accusation.t = accusation;
        guesses :=
          (
            switch (outcome) {
            | Normal(Held(_))
            | Final(Lose) =>
              Js.log("removing");
              Js.log(guess);
              GuessSet.remove(guess, guesses^);
            | Normal(Unheld)
            | Final(Win) => guesses^
            }
          );
      },
      accusationActions,
    );

    guesses^;
  };

  let f = state => {
    let {categories, hiddenItems, history, startingPlayers} = state;
    let allGuesses = getAllPossibleGuesses(categories);
    let hiddenItems =
      switch (hiddenItems) {
      | Some(items) => items
      | None => ItemSet.empty
      };

    let player = Turn.controlledPlayer(history, startingPlayers);
    let playerItems =
      switch (player.items) {
      | Controlled(items) => items
      | Uncontrolled(_) => raise(Impossible)
      };

    let filtered =
      allGuesses
      |> filterBasedOnItems(hiddenItems)
      |> filterBasedOnItems(playerItems)
      |> filterBasedOnHistory(history);

    Js.log("all");
    Js.log(allGuesses |> GuessSet.to_array);
    Js.log(GuessSet.cardinal(allGuesses));
    Js.log("filtered");
    Js.log(filtered |> GuessSet.to_array);
    Js.log(GuessSet.cardinal(filtered));
    Js.log("--------");
    let chosen = GuessSet.choose(filtered);
    (chosen, GuessSet.cardinal(filtered) == 1);
  };

  let engine = {name: "v1", f};
};