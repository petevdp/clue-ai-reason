// Entry point

[@bs.val] external document: Js.t({..}) = "document";

// We're using raw DOM manipulations here, to avoid making you read
// ReasonReact when you might precisely be trying to learn it for the first
ReactDOMRe.render(
  <ClueComponent
    categories=Clue.startingCategories
    playerNames=Clue.startingPlayers
  />,
  document##getElementById("react-root"),
);