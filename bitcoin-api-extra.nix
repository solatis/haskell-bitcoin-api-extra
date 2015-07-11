{ mkDerivation, base, binary, bitcoin-api, bitcoin-block
, bitcoin-tx, bytestring, conduit, hspec, http-client, lens, stdenv
, stm, stm-chans, stm-conduit, text, transformers, wreq
}:
mkDerivation {
  pname = "bitcoin-api-extra";
  version = "0.9.1";
  src = ./.;
  buildDepends = [
    base binary bitcoin-api bitcoin-block bitcoin-tx bytestring conduit
    lens stm stm-chans stm-conduit text transformers
  ];
  testDepends = [
    base bitcoin-api bitcoin-tx bytestring conduit hspec http-client
    lens text wreq
  ];
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Higher level constructs on top of the bitcoin-api package";
  license = stdenv.lib.licenses.mit;
}
