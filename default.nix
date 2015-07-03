{ mkDerivation, aeson, attoparsec, base, bytestring, conduit-extra
, either, email, esqueleto, HaskellNet, monad-control, monad-logger
, monad-loops, network, persistent, persistent-postgresql
, persistent-template, pwstore-fast, servant, servant-client
, servant-server, smtp, stdenv, text, time, transformers, warp
, yaml
}:
mkDerivation {
  pname = "dnolist";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base bytestring conduit-extra either email
    esqueleto HaskellNet monad-control monad-logger monad-loops network
    persistent persistent-postgresql persistent-template pwstore-fast
    servant servant-client servant-server smtp text time transformers
    warp yaml
  ];
  license = stdenv.lib.licenses.bsd3;
}
