// open Emacs__Component;
// let parse: string => array(WarningError.t) =
//   raw => {
//     let lines = raw |> Js.String.split("\n");
//     lines
//     |> Util.Dict.partite(((_, i)) => i === 0 ? Some("errors") : None)
//     |> Emacs__Parser.partiteWarningsOrErrors("errors")
//     |> Js.Dict.get(_, "errors")
//     |> mapOr(
//          metas =>
//            metas
//            |> Array.map(WarningError.parseError)
//            |> Array.filterMap(x => x),
//          [||],
//        );
//   };
// [@react.component]
// let make = (~payload: string) =>
//   <ul>
//     {parse(payload)
//      |> Array.mapi((value, i) =>
//           <WarningError key={string_of_int(i)} value />
//         )
//      |> React.array}
//   </ul>;
