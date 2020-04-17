module R = React;

let trueElseHide = (show, element) =>
  show
    ? element : <span className="font-italic"> {R.string("hidden")} </span>;