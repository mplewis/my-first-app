open ReasonReact;

let s = stringToElement;

type action =
  | Tick;

type state = {count: int};

let component = reducerComponent("Counter");

let factors = n => {
  let rec range =
    fun
    | 0 => []
    | n => range(n - 1) @ [n];
  List.filter(v => n mod v == 0, range(n));
};

let rec fib =
  fun
  | 0 => 0
  | 1 => 1
  | n => fib(n - 1) + fib(n - 2);

let rec range =
  fun
  | 0 => [0]
  | n => range(n - 1) @ [n];

let fibs = n => range(n) |> List.map(fib);

let stringifyInts = ints =>
  ints |> List.map(string_of_int) |> String.concat(", ");

let sFn = (fn, n) => n |> fn |> stringifyInts;

let sFactors = sFn(factors);

let sRange = sFn(range);

let sFibs = sFn(fibs);

let make = _children => {
  ...component,
  initialState: () => {count: 0},
  reducer: (action, state) =>
    switch action {
    | Tick => Update({count: state.count + 1})
    },
  subscriptions: self => [
    Sub(
      () => Js.Global.setInterval(() => self.send(Tick), 500),
      Js.Global.clearInterval
    )
  ],
  render: ({state}) =>
    <div>
      <p> (s(string_of_int(state.count))) </p>
      <p> (s(sFactors(state.count))) </p>
      <p> (s(sRange(state.count))) </p>
      <p> (s(sFibs(state.count))) </p>
    </div>
  /* <p> (s(string_of_int(fib(state.count)))) </p> */
};
