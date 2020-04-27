// Entry point

[@bs.val] external document: Js.t({..}) = "document";

open Clue;
open Utils;

let engines = Engine.[Random.engine, V1.engine];
let selectedEngine = "v1";

let controlledPlayerName = "alice";
let controlledPlayerItems = ItemSet.empty;

let startingPlayers =
  [|("alice", 4), ("bob", 4), ("carol", 4), ("dan", 4)|]
  |> Array.map(((name, numItems)) => {
       let playerItems =
         name == controlledPlayerName
           ? Player.Controlled({
               items: controlledPlayerItems,
               engineName: selectedEngine,
             })
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

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
ReactDOMRe.render(
  <ClueComponent
    numHiddenItems=2
    categories=startingCategories
    specifiedPlayers=startingPlayers
    engines
  />,
  document##getElementById("react-root"),
);