[@bs.module "util"]
external promisify:
  (('a, (option(Js.Exn.t), 'b) => unit) => unit) =>
  (. 'a) => Js.Promise.t('b) =
  "promisify";
