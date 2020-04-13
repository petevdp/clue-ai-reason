module R = React;

let hide = (show, element) =>
  show
    ? element : <span className="font-italic"> {R.string("hidden")} </span>;