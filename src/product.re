module Option = {
  let unwrapUnsafely = (data) =>
    switch data {
    | Some(v) => v
    | None => raise(Invalid_argument("unwrapUnsafely called on None"))
    };
};

type order = {
  price: string,
  size: string,
  numOrders: float
};

type orderBook = {
  bids: array(order),
  asks: array(order)
};

type state = {
  orderBook,
  isOpen: bool
};

type action =
  | ToggleOpen
  | FetchOrders(orderBook);

let parseOrder = (json: array(Js.Json.t)) : order => {
  let get = Array.get(json);
  {
    price: get(0) |> Js.Json.decodeString |> Option.unwrapUnsafely,
    size: get(1) |> Js.Json.decodeString |> Option.unwrapUnsafely,
    numOrders: get(2) |> Js.Json.decodeNumber |> Option.unwrapUnsafely
  }
};

let parseOrders = (json: array(Js.Json.t)) : array(order) =>
  json
  |> Js.Array.map(
       (o) => o |> Js.Json.decodeArray |> Option.unwrapUnsafely |> parseOrder
     );

let component = ReasonReact.reducerComponent("Product");

let make =
    (
      ~id,
      ~display_name,
      /* ~base_currency,
         ~quote_currency,
         ~base_min_size,
         ~base_max_size,
         ~quote_increment,
         ~display_name,
         ~status,
         ~margin_enabled, */
      _children
    ) => {
  let click = (event, self) => {
    if (Array.length(self.ReasonReact.state.orderBook.bids) == 0
        || Array.length(self.ReasonReact.state.orderBook.asks) == 0) {
      Js.Promise.(
        Fetch.fetch("https://api.gdax.com/products/" ++ id ++ "/book?level=2")
        |> then_(Fetch.Response.json)
        |> then_((json) => Js.Json.decodeObject(json) |> resolve)
        |> then_((opt) => Option.unwrapUnsafely(opt) |> resolve)
        |> then_(
             (obj) => {
               let decode = (key) =>
                 Js.Dict.get(obj, key)
                 |> Option.unwrapUnsafely
                 |> Js.Json.decodeArray
                 |> Option.unwrapUnsafely;
               (decode("bids"), decode("asks")) |> resolve
             }
           )
        |> then_(
             (orders) =>
               {
                 let (bids, asks) = orders;
                 {bids: parseOrders(bids), asks: parseOrders(asks)}
               }
               |> resolve
           )
        |> then_(
             (orders) =>
               self.ReasonReact.reduce((_) => FetchOrders(orders), ())
               |> resolve
           )
        |> catch((err) => Js.log(err) |> resolve)
      )
      |> ignore
    };
    self.ReasonReact.reduce((_) => ToggleOpen, ()) |> ignore
  };
  {
    ...component,
    initialState: () => {orderBook: {bids: [||], asks: [||]}, isOpen: false},
    reducer: (action, state) =>
      switch action {
      | ToggleOpen => ReasonReact.Update({...state, isOpen: ! state.isOpen})
      | FetchOrders(orders) =>
        ReasonReact.Update({...state, isOpen: true, orderBook: orders})
      },
    render: (_self) => {
      let renderOrders = (orders) =>
        if (! _self.state.isOpen) {
          ReasonReact.nullElement
        } else {
          <ol>
            (
              ReasonReact.arrayToElement(
                Array.map(
                  (o) => <li> (ReasonReact.stringToElement(o.price)) </li>,
                  orders
                )
              )
            )
          </ol>
        };
      let bidList = renderOrders(_self.state.orderBook.bids);
      let askList = renderOrders(_self.state.orderBook.asks);
      <div>
        <p onClick=(_self.handle(click))>
          (ReasonReact.stringToElement(display_name))
        </p>
        bidList
        askList
      </div>
    }
  }
};
