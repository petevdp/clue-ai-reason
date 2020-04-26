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

module Player = {
  type playerItems =
    | Controlled(ItemSet.t)
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
    | Controlled(set) => ItemSet.cardinal(set)
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

module Engine = {
  type reducedState = {
    history: list(Turn.t),
    startingPlayers: array(Player.t),
    categories: array(Category.t),
  };
  type action = Player.action;
  type t = reducedState => Player.action;

  module Random = {
    let t: t =
      state => {
        let final = Belt.Array.shuffle([|true, false|])[0];
        let {categories} = state;
        open Category;
        let guess: Guess.t = {
          location: categories |> getByName("location") |> randomItem,
          weapon: categories |> getByName("weapon") |> randomItem,
          suspect: categories |> getByName("suspect") |> randomItem,
        };

        final ? Player.FinalAction(guess) : Player.NormalAction(guess);
      };
  };
};

type state = State.t;

let controlledPlayerName = "alice";
let controlledPlayerItems = ItemSet.empty;
let engine = Engine.Random.t;

let startingPlayers =
  [|("alice", 4), ("bob", 4), ("carol", 4), ("dan", 4)|]
  |> Array.map(((name, numItems)) => {
       let playerItems =
         name == controlledPlayerName
           ? Player.Controlled(controlledPlayerItems)
           : Player.Uncontrolled(numItems);
       (name, playerItems);
     });

let startingCategories: array(Category.t) = [|
  {
    name: "locations",
    itemType: Location,
    itemNames:
      StringSet.of_list([
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
    itemType: Weapon,
    itemNames:
      StringSet.of_list(["hammer", "chainsaw", "butcher knife", "pencil"]),
  },
  {
    name: "suspects",
    itemType: Suspect,
    itemNames:
      StringSet.of_list(["dirty harry", "cool hand luke", "ted cruz", "OJ"]),
  },
|];