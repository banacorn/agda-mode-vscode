module ExtensionContext = {
    type t;
};

module Disposable = {
    type t;
}

module Commands = {
    [@bs.module "vscode"] [@bs.scope ("commands")]
    external registerCommand: (string, 'a => unit) => Disposable.t = "registerCommand";
    
};

module Window = {
    [@bs.module "vscode"] [@bs.scope ("window")]
    external showInformationMessage: (string) => unit = "showInformationMessage";
    
};
