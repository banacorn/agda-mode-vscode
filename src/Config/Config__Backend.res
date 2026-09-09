type t = GHC | GHCNoMain | LaTeX | QuickLaTeX | JS | HTML | Dot

let decode = (raw: option<string>): t =>
  switch raw {
  | Some("GHC") => GHC
  | Some("GHCNoMain") => GHCNoMain
  | Some("LaTeX") => LaTeX
  | Some("QuickLaTeX") => QuickLaTeX
  | Some("JS") => JS
  | Some("HTML") => HTML
  | Some("Dot") => Dot
  | _ => GHCNoMain
  }

let encode = (backend: t): string =>
  switch backend {
  | GHC => "GHC"
  | GHCNoMain => "GHCNoMain"
  | LaTeX => "LaTeX"
  | QuickLaTeX => "QuickLaTeX"
  | JS => "JS"
  | HTML => "HTML"
  | Dot => "Dot"
  }
