open Utils;
open ComponentUtils;

type accusationFormValues = StringMap.t(option(Clue.item));

module Style = {
  open Css;
  let categoryInputList = style([display(`flex)]);
  let container =
    style([
      width(`percent(100.0)),
      display(`flex),
      flexDirection(`column),
      alignItems(`center),
    ]);

  let submitButton = "btn btn-primary";
};

[@react.component]
let make =
    (
      ~dispatch: ClueReducer.action => unit,
      ~categories: array(Clue.Category.t),
      ~accusationForm,
      ~currentPlayerIndex,
    ) => {
  open Clue.Category;
  let {guessChoice, final}: Clue.Accusation.form = accusationForm;

  let inputElements =
    categories
    |> Array.map(category => {
         let selectedItem =
           switch (category.itemType) {
           | Location => guessChoice.locationChoice
           | Weapon => guessChoice.weaponChoice
           | Suspect => guessChoice.suspectChoice
           };
         <CategoryInputComponent
           key={category.name}
           category
           selectedItem
           dispatch
         />;
       });

  let canSubmit =
    Clue.Accusation.accusationFromForm(accusationForm, currentPlayerIndex)
    != None;
  let onClickSubmitTurn = e => {
    ReactEvent.Mouse.preventDefault(e);
    dispatch(SubmitAccusation);
  };

  let onFinalCheckboxChange = _ =>
    dispatch(AccusationFormChange(ToggleFinal));

  <form className="form">
    <div className="container panel">
      <div className="row"> {R.array(inputElements)} </div>
    </div>
    <div className="col-]md-12">
      <button
        type_="button"
        disabled={!canSubmit}
        onClick=onClickSubmitTurn
        className=Style.submitButton>
        {R.string("Make Turn")}
      </button>
      <span>
        <label> {R.string("Final: ")} </label>
        <input type_="checkbox" checked=final onChange=onFinalCheckboxChange />
      </span>
    </div>
  </form>;
};