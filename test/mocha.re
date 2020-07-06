exception UnhandledPromise;

exception UnhandledPromise2;

let run = () => {
  Js.Promise.make((~resolve, ~reject) => {reject(. UnhandledPromise2)});
};
