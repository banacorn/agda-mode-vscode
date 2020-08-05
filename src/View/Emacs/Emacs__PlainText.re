open React;

[@react.component]
let make = (~payload: string) => <p> {string(payload)} </p>;
