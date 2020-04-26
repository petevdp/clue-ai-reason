module R = React;

let str = R.string;

let trueElseHide = (show, element) =>
  show
    ? element : <span className="font-italic"> {R.string("hidden")} </span>;