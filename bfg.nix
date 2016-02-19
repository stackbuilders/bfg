{ mkDerivation, aeson, base, blaze-html, either, monad-logger, mtl
, pandoc, persistent, persistent-sqlite, persistent-template
, servant-blaze, servant-docs, servant-pandoc, servant-server
, stdenv, text, wai, wai-extra, warp
}:
mkDerivation {
  pname = "bfg";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html either monad-logger mtl pandoc persistent
    persistent-sqlite persistent-template servant-blaze servant-docs
    servant-pandoc servant-server text wai wai-extra warp
  ];
  license = stdenv.lib.licenses.unfree;
}
