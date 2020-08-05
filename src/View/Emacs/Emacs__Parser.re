open Belt;

module Dict = {
  open Js.Dict;
  let partite =
      (xs: array('a), tagEntry: (('a, int)) => option(string))
      : t(array('a)) => {
    let keys: array((key, int)) =
      xs
      ->Array.mapWithIndex((i, x) => (i, x)) /* zip with index */
      ->Array.keepMap(((i, x)) =>
          tagEntry((x, i))->Option.map(key => (key, i))
        );
    let intervals: array((key, int, int)) =
      keys->Array.mapWithIndex((n, (key, index)) =>
        switch (keys[n + 1]) {
        | Some((_, next)) => (key, index, next)
        | None => (key, index, Array.length(xs))
        }
      );
    intervals->Array.map(((key, start, end_)) =>
      (key, xs->Js.Array.slice(~start, ~end_))
    )
    |> fromArray;
  };
  /* split an entry */
  let split =
      (dict: t('a), key: key, splitter: 'a => t('a)): t(array(string)) =>
    switch (get(dict, key)) {
    | Some(value) =>
      /* insert new entries */
      entries(splitter(value))
      ->Array.forEach(((k, v)) => set(dict, k, v));
      dict;
    | None => dict
    };
  let update = (dict: t('a), key: key, f: 'a => 'a): t('a) =>
    switch (get(dict, key)) {
    | Some(value) =>
      set(dict, key, f(value));
      dict;
    | None => dict
    };
};

module Array_ = {
  let partite = (xs: array('a), p: 'a => bool): array(array('a)) => {
    let indices: array(int) =
      xs
      ->Array.mapWithIndex((i, x) => (i, x)) /* zip with index */
      ->Array.keep(((_, x)) => p(x)) /* filter bad indices out */
      ->Array.map(fst); /* leave only the indices */
    /* prepend 0 as the first index */
    let indicesWF: array(int) =
      switch (indices[0]) {
      | Some(n) => n === 0 ? indices : Array.concat(indices, [|0|])
      | None => Array.length(indices) === 0 ? [|0|] : indices
      };
    let intervals: array((int, int)) =
      indicesWF->Array.mapWithIndex((n, index) =>
        switch (indicesWF[n + 1]) {
        | Some(next) => (index, next)
        | None => (index, Array.length(xs))
        }
      );
    intervals->Array.map(((start, end_)) =>
      xs |> Js.Array.slice(~start, ~end_)
    );
  };
  let mergeWithNext:
    (array(array('a)), array('a) => bool) => array(array('a)) =
    (xs, p) =>
      xs->Array.reduce(
        [||],
        (acc, x) => {
          let last = acc[Array.length(acc) - 1];
          switch (last) {
          | None => [|x|]
          | Some(l) =>
            if (p(l)) {
              (acc[Array.length(acc) - 1] = Array.concat(x, l)) |> ignore;
              acc;
            } else {
              Array.concat([|x|], acc);
            }
          };
        },
      );
};

let unindent: array(string) => array(string) =
  lines => {
    let isNewline = (line, nextLine) => {
      let sort = [%re "/^Sort \\S*/"];
      let delimeter = [%re "/^\\u2014{4}/g"];
      /* banana : Banana */
      let completeJudgement = [%re
        "/^(?:(?:[^\\(\\{\\s]+\\s+\\:=?)|Have\\:|Goal\\:)\\s* \\S*/"
      ];
      /* case when the term's name is too long, the rest of the judgement
            would go to the next line, e.g:
                 banananananananananananananananana
                     : Banana
         */
      let reallyLongTermIdentifier = [%re "/^\\S+$/"];
      let restOfTheJudgement = [%re "/^\\s*\\:=?\\s* \\S*/"];
      Js.Re.test_(sort, line)
      || Js.Re.test_(delimeter, line)
      || Js.Re.test_(reallyLongTermIdentifier, line)
      && nextLine->Option.mapWithDefault(false, line =>
           Js.Re.test_(restOfTheJudgement, line)
         )
      || Js.Re.test_(completeJudgement, line);
    };
    let newLineIndices: array(int) =
      lines
      ->Array.mapWithIndex((index, line) => (line, lines[index + 1], index))
      ->Array.keep(((line, nextLine, _)) => isNewline(line, nextLine))
      ->Array.map(((_, _, index)) => index);
    newLineIndices
    ->Array.mapWithIndex((i, index) =>
        switch (newLineIndices[i + 1]) {
        | None => (index, Array.length(lines) + 1)
        | Some(n) => (index, n)
        }
      )
    ->Array.map(((start, end_)) =>
        lines |> Js.Array.slice(~start, ~end_) |> Js.Array.joinWith("\n")
      );
  };

let partiteMetas = xs =>
  xs->Dict.split("metas", (rawMetas: array(string)) => {
    let metas = unindent(rawMetas);
    let indexOfHiddenMetas =
      metas->Array.getIndexBy(s =>
        Component.Output.parseOutputWithRange(s)->Option.isSome
      );
    metas->Dict.partite(((_, i)) =>
      switch (indexOfHiddenMetas) {
      | Some(n) =>
        if (i === n) {
          Some("hiddenMetas");
        } else if (i === 0) {
          Some("interactionMetas");
        } else {
          None;
        }
      | None =>
        /* All interaction metas */
        if (i === 0) {
          Some("interactionMetas");
        } else {
          None;
        }
      }
    );
  });

let partiteWarningsOrErrors = (xs, key) =>
  xs->Dict.update(
    key,
    (raw: array(string)) => {
      let hasDelimeter =
        raw[0]
        ->Option.flatMap(Js.String.match([%re "/^\\u2014{4}/"]))
        ->Option.isSome;
      let lines = hasDelimeter ? raw |> Js.Array.sliceFrom(1) : raw;
      let markWarningStart = line => line->View.Range.parse->Option.isSome;
      /* If the previous warning of error ends with "at", then we have to glue it back */
      let glueBack = xs =>
        xs[Array.length(xs) - 1]
        ->Option.flatMap(Js.String.match([%re "/at$/"]))
        ->Option.isSome;
      lines
      ->Array_.partite(markWarningStart)
      ->Array_.mergeWithNext(glueBack)
      ->Array.map(Js.Array.joinWith("\n"));
    },
  );
