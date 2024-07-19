{ mkDerivation, aeson, base, base64-bytestring, binary-instances
, bytestring, extra, lib, shake, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "fdroid-build";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base base64-bytestring binary-instances bytestring extra
    shake text transformers unordered-containers vector
  ];
  license = lib.licenses.lgpl21Plus;
  mainProgram = "fdroid-build";
}
