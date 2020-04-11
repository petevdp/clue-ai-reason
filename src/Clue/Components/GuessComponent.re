open Utils;
open ComponentUtils;

[@react.component]
let make = (~guess: Clue.Guess.t) => {
  let pairElementMap =
    guess
    |> StringMap.bindings
    |> Array.of_list
    |> Array.map(((categoryName, item)) => {
         <li key=categoryName>
           <pair>
             <key> {R.string(categoryName)} </key>
             {R.string(" : ")}
             <value> {R.string(item)} </value>
           </pair>
         </li>
       });

  <ul> {R.array(pairElementMap)} </ul>;
};