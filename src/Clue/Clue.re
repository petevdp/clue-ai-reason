open GlobalTypes;
open Utils;

type location = string;
type weapon = string;
type suspect = string;
module Item = {
  type itemType =
    | Location
    | Weapon
    | Suspect;
  type t = {
    itemType,
    name: string,
  };
  let compare = (a, b) => {
    String.compare(a.name, b.name);
  };

  let typeAsString = itemType =>
    switch (itemType) {
    | Location => "Location"
    | Weapon => "Weapon"
    | Suspect => "Suspect"
    };
};
type item = Item.t;

module ItemSet = {
  include Set(Item);
  let findByName = (name, t) => {
    find_first(elt => {elt.name == name}, t);
  };
  let findByNameOpt = (name, t) => {
    find_first_opt(elt => {elt.name == name}, t);
  };

  let names = t => {
    fold(
      (item, names) => StringSet.add(item.name, names),
      t,
      StringSet.empty,
    );
  };
};
module Guess = {
  type t = {
    location,
    weapon,
    suspect,
  };

  type form = {
    locationChoice: option(location),
    weaponChoice: option(weapon),
    suspectChoice: option(suspect),
  };

  let toStringMap = t =>
    StringMap.(
      empty
      |> add("location", t.location)
      |> add("weapon", t.weapon)
      |> add("suspect", t.suspect)
    );

  let compare = (a, b) => {
    let comparisons = [
      String.compare(a.location, b.location),
      String.compare(a.weapon, b.weapon),
      String.compare(a.suspect, b.suspect),
    ];

    try(comparisons |> List.filter(comp => comp != 0) |> List.hd) {
    | Not_found => 0
    };
  };

  let emptyForm = {
    locationChoice: None,
    weaponChoice: None,
    suspectChoice: None,
  };

  let canFormBeSubmitted = form =>
    switch (form.locationChoice, form.weaponChoice, form.suspectChoice) {
    | (Some(_), Some(_), Some(_)) => true
    | _ => false
    };

  let items = t => {
    Item.(
      ItemSet.(
        empty
        |> add({name: t.location, itemType: Location})
        |> add({name: t.weapon, itemType: Weapon})
        |> add({name: t.suspect, itemType: Suspect})
      )
    );
  };
};

type guess = Guess.t;
module GuessSet = {
  include Set(Guess);
};

module Player = {
  type controlledPlayerExtraInfo = {
    items: ItemSet.t,
    engineName: string,
  };

  type playerItems =
    | Controlled(controlledPlayerExtraInfo)
    | Uncontrolled(int);

  type t = {
    name: string,
    lost: bool,
    items: playerItems,
  };

  type specifiedPlayer = (string, playerItems);

  let initPlayer = ((name, playerItems)) => {
    name,
    lost: false,
    items: playerItems,
  };

  let numItems = ({items}) => {
    switch (items) {
    | Controlled({items}) => ItemSet.cardinal(items)
    | Uncontrolled(num) => num
    };
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
    itemType: Item.itemType,
    name: string,
    itemNames: StringSet.t,
  };

  let randomItem = t => t.itemNames |> StringSet.randomItem;

  let getByName = (name: string, arr: array(t)) => {
    switch (Belt.Array.getBy(arr, c => c.name == name)) {
    | Some(category) => category
    | None => raise(NotFound)
    };
  };

  let get = (itemType: Item.itemType, arr: array(t)) => {
    switch (Belt.Array.getBy(arr, c => c.itemType == itemType)) {
    | Some(category) => category
    | None => raise(NotFound)
    };
  };

  let to_map = t => {
    Array.fold_left(
      (map, {name, itemNames}) => {StringMap.add(name, itemNames, map)},
      StringMap.empty,
      t,
    );
  };
  let itemfromCategoryAndName = ({itemType}, name): item => {
    name,
    itemType,
  };

  let allItems = (categories: array(t)) => {
    Array.fold_left(
      (set, {itemType, itemNames}) => {
        StringSet.fold(
          (name, itemSet) => {ItemSet.add({name, itemType}, itemSet)},
          itemNames,
          set,
        )
      },
      ItemSet.empty,
      categories,
    );
  };
};

module Accusation = {
  type t = {
    playerIndex: index,
    guess: Guess.t,
    final: bool,
  };
  type form = {
    guessChoice: Guess.form,
    final: bool,
  };

  type formChange =
    | ItemValue(Item.t)
    | ToggleFinal;

  let emptyForm = {guessChoice: Guess.emptyForm, final: false};

  let accusationFromForm = (form, playerIndex: index) =>
    switch (form.guessChoice) {
    | {
        locationChoice: Some(location),
        weaponChoice: Some(weapon),
        suspectChoice: Some(suspect),
      } =>
      Some({
        playerIndex,
        final: form.final,
        guess: {
          location,
          weapon,
          suspect,
        },
      })
    | _ => None
    };
};
type accusation = Accusation.t;

module Turn = {
  type normalOutcome =
    | Held(index)
    | Unheld;

  type finalOutcome =
    | Win
    | Lose;

  type outcome =
    | Normal(normalOutcome)
    | Final(finalOutcome);

  type normalOutcomeForm = option(normalOutcome);
  type finalOutcomeForm = option(finalOutcome);
  type turnOutcomeForm =
    | NormalOutcomeForm(normalOutcomeForm)
    | FinalOutcomeForm(finalOutcomeForm);

  type turnPhase =
    | Start
    | PendingAccusation(Accusation.form)
    | PendingShowHidden(array(option(item)))
    | PendingShowHiddenUncontrolled
    | PendingTurnOutcome(accusation);

  type accusationAction = {
    outcome,
    accusation,
  };

  type turnActionChoice =
    | AccusationChoice
    | ShowHiddenChoice;

  type turnAction =
    | Accusation(accusationAction)
    | ShowHidden;

  type t = {
    playerIndex: index,
    players: array(Player.t),
    turnAction,
  };

  let startingPhase = Some(Start);
  let startingPendingOutcome = accusation => PendingTurnOutcome(accusation);

  let winning = t =>
    switch (t.turnAction) {
    | Accusation({outcome: Final(Win)}) => true
    | _ => false
    };

  exception InvalidGuessForm;

  let currentPlayers = (history, startingPlayers) =>
    switch (List.last(history)) {
    | None => startingPlayers
    | Some({players}) => players
    };

  exception NoPlayersLeft;

  let currentPlayerIndex = history => {
    switch (List.last(history)) {
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

  exception IncompleteTurn;
  let isGameEndingTurn = t => {
    switch (t.turnAction) {
    | Accusation({outcome: Final(outcome)}) =>
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
    | _ => false
    };
  };
};

module State = {
  type t = {
    history: list(Turn.t),
    turnPhase: option(Turn.turnPhase),
    startingPlayers: array(Player.t),
    categories: array(Category.t),
    controlledPlayerIndex: index,
    hiddenItems: option(ItemSet.t),
    numHiddenItems: int,
  };

  type gamePhase =
    | Playing(index)
    | Ended(index);

  let currentPlayers = t => Turn.currentPlayers(t.history, t.startingPlayers);
  let isControlledPlayerTurn = ({history, controlledPlayerIndex}) => {
    Turn.currentPlayerIndex(history) == controlledPlayerIndex;
  };
  let getControlledPlayerInfo = t => {
    exception ControlledPlayerIndexIsUncontrolled;
    switch (currentPlayers(t)[t.controlledPlayerIndex]) {
    | {items: Player.Controlled(info)} => info
    | _ => raise(ControlledPlayerIndexIsUncontrolled)
    };
  };

  exception NotEnoughPlayers;
  let determineGamePhase = t => {
    let players = currentPlayers(t);
    let lastTurn = List.last(t.history);
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

  let initialize =
      (
        categories: array(Category.t),
        specifiedPlayers: array(Player.specifiedPlayer),
        numHiddenItems,
      )
      : t => {
    let startingPlayers = Array.map(Player.initPlayer, specifiedPlayers);

    let (controlledPlayerIndex, _) =
      startingPlayers
      |> Array.to_list
      |> List.mapi((i, p) => (i, p))
      |> List.find(((_, p): (int, Player.t)) =>
           switch (p.items) {
           | Player.Controlled(_) => true
           | Player.Uncontrolled(_) => false
           }
         );

    {
      startingPlayers,
      turnPhase: Turn.startingPhase,
      categories,
      history: [],
      hiddenItems: None,
      controlledPlayerIndex,
      numHiddenItems,
    };
  };
};

type state = State.t;
module Engine = {
  type reducedState = {
    history: list(Turn.t),
    startingPlayers: array(Player.t),
    hiddenItems: option(ItemSet.t),
    categories: array(Category.t),
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
  };

  let getEngineByName = (name, engines) =>
    engines |> Array.to_list |> List.filter(e => e.name == name);

  let useEngine = (engines: list(t), state: State.t, makeMove) => {
    React.useEffect3(
      () => {
        let controlledPlayer = State.getControlledPlayerInfo(state);
        let engine =
          List.find(e => e.name == controlledPlayer.engineName, engines);
        let isAccusationPhase =
          switch (state.turnPhase) {
          | Some(Turn.PendingAccusation(_)) => true
          | _ => false
          };

        if (State.isControlledPlayerTurn(state) && isAccusationPhase) {
          let _ =
            Js.Global.setTimeout(
              () => state |> getReducedState |> engine.f |> makeMove,
              1,
            );
          ();
        };
        None;
      },
      (state, engines, makeMove),
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

    let filterBasedOnHidden = (hiddenItems, allGuesses) =>
      switch (hiddenItems) {
      | Some(items) =>
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
      | None => allGuesses
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
          let {guess}: Accusation.t = accusation;
          guesses :=
            (
              switch (outcome) {
              | Normal(Held(_))
              | Final(Lose) => GuessSet.remove(guess, guesses^)
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
      let {categories, hiddenItems, history} = state;
      let allGuesses = getAllPossibleGuesses(categories);
      let filtered =
        allGuesses
        |> filterBasedOnHidden(hiddenItems)
        |> filterBasedOnHistory(history);

      Js.log("all");
      Js.log(allGuesses |> GuessSet.to_array);
      Js.log(GuessSet.cardinal(allGuesses));
      Js.log("filtered");
      Js.log(filtered |> GuessSet.to_array);
      Js.log(GuessSet.cardinal(filtered));
      Js.log("--------");
      let chosen = GuessSet.choose(filtered);
      (chosen, false);
    };

    let engine = {name: "v1", f};
  };
};