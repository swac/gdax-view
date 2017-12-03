[%bs.raw {|require('./app.css')|}];

module Option = {
  let unwrapUnsafely = (data) =>
    switch data {
    | Some(v) => v
    | None => raise(Invalid_argument("unwrapUnsafely called on None"))
    };
};

type product = {
  id: string,
  base_currency: string,
  quote_currency: string,
  base_min_size: string,
  base_max_size: string,
  quote_increment: string,
  display_name: string,
  status: string,
  margin_enabled: bool
};

let parseProduct = (json: Js.Json.t) : product =>
  Json.Decode.{
    id: field("id", string, json),
    base_currency: field("base_currency", string, json),
    quote_currency: field("quote_currency", string, json),
    base_min_size: field("base_min_size", string, json),
    base_max_size: field("base_max_size", string, json),
    quote_increment: field("quote_increment", string, json),
    display_name: field("display_name", string, json),
    status: field("status", string, json),
    margin_enabled: field("margin_enabled", bool, json)
  };

type action =
  | FetchProducts(array(product));

let component = ReasonReact.reducerComponent("App");

let make = (~message, _children) => {
  ...component,
  initialState: () => [||],
  didMount: (_self) => {
    Js.Promise.(
      Fetch.fetch("https://api.gdax.com/products")
      |> then_(Fetch.Response.json)
      |> then_((json) => Js.Json.decodeArray(json) |> resolve)
      |> then_((opt) => Option.unwrapUnsafely(opt) |> resolve)
      |> then_(
           (arr) => arr |> Js.Array.map((i) => parseProduct(i)) |> resolve
         )
      |> then_((arr) => _self.reduce((_) => FetchProducts(arr), ()) |> resolve)
    )
    |> ignore;
    ReasonReact.NoUpdate
  },
  reducer: (action, _) =>
    switch action {
    | FetchProducts(arr) => ReasonReact.Update(arr)
    },
  render: (_self) =>
    <div className="App">
      <div className="App-header">
        <h2> (ReasonReact.stringToElement(message)) </h2>
      </div>
      (
        ReasonReact.arrayToElement(
          Array.map(
            (i) => <Product id=i.id display_name=i.display_name />,
            _self.state
          )
        )
      )
    </div>
};
