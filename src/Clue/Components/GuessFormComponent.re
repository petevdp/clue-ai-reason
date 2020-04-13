open Utils;
open ComponentUtils;

type turnFormValues = StringMap.t(option(Clue.item));

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
      ~turnForm,
    ) => {
  let {values, final}: Clue.Turn.form = turnForm;

  let inputElements =
    values
    |> StringMap.bindings
    |> Array.of_list
    |> Array.map(((key, selectedItem)) => {
         let category = Clue.Category.getByName(key, categories);
         <CategoryInputComponent key category selectedItem dispatch />;
       });
  let canSubmit = Clue.Turn.canFormBeSubmitted(turnForm);
  let onClickSubmitTurn = e => {
    ReactEvent.Mouse.preventDefault(e);
    dispatch(SubmitTurn);
  };

  let onFinalCheckboxChange = _ =>
    dispatch(ClueReducer.(TurnFormChange(ToggleFinal)));

  <form className="form">
    <div className="container panel">
      <div className="row"> {R.array(inputElements)} </div>
    </div>
    <div className="col-md-12">
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