{ mkDerivation, aeson, base, base64-bytestring, binary-instances
, bytestring, extra, http-client, http-client-tls, lib, shake, text
, transformers, unordered-containers
}:
mkDerivation {
  pname = "fdroid-build";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base64-bytestring binary-instances bytestring extra
    http-client http-client-tls shake text transformers
    unordered-containers
  ];
  license = lib.licenses.lgpl21Plus;
  mainProgram = "fdroid-build";
}
