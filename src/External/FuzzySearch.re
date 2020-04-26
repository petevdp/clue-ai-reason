type state('a) = {
  selectedIndex: int,
  results: array('a),
};

type styles = {
  selectedResultStyle: ReactDOMRe.style,
  resultsStyle: ReactDOMRe.style,
};

type clickHandler = ReactEvent.Mouse.t => unit;

type resultsTemplate('a) =
  (int, state('a), styles, clickHandler) => React.element;

[@bs.module "@storybook/react-fuzzy"] [@react.component]
external make:
  (
    ~list: array('a),
    ~keys: array(string),
    ~width: int,
    ~onSelect: 'a => unit,
    ~resultsTemplate: resultsTemplate('a)
  ) =>
  React.element =
  "default";