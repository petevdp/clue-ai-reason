open Utils;
open ComponentUtils;

[@react.component]
let make = (~guess: Clue.Guess.t) => {
  open Clue.Guess;
  let pairElementMap =
    guess
    |> toStringMap
    |> StringMap.bindings
    |> Array.of_list
    |> Array.map(((categoryName, item)) => {
         <li key=categoryName>
           <strong> {R.string(categoryName)} </strong>
           {R.string(" : ")}
           {R.string(item)}
         </li>
       });

  <ul> {R.array(pairElementMap)} </ul>;
};