# libghostty-vt from its signed release tarballs (PACKAGING-LIBGHOSTTY.md):
# the source and the Zig packages it builds against, published together.
# The hashes are of the tarballs whose minisign signatures were checked
# against Ghostty's release key.
{ lib, stdenv, fetchurl, runCommand, pkg-config, zig }:
let
  version = "1.0.0";
  release = "https://release.files.ghostty.org/libghostty-vt/${version}";
  deps = runCommand "libghostty-vt-${version}-deps" {
    src = fetchurl {
      url = "${release}/libghostty-vt-${version}-deps.tar.gz";
      hash = "sha256-TV6l5Ejkx+qPtVzLqoZo8ql9RPQ4zugx4rfs0WLYXCk=";
    };
  } ''
    mkdir $out
    tar -xzf $src -C $out --strip-components=1
  '';
in
stdenv.mkDerivation {
  pname = "libghostty-vt";
  inherit version;

  src = fetchurl {
    url = "${release}/libghostty-vt-${version}.tar.gz";
    hash = "sha256-Sm73WL5yGSPhlp57gbRD3MktjdP7WjlgDAOogXp1NnY=";
  };

  __structuredAttrs = true;
  strictDeps = true;

  nativeBuildInputs = [ pkg-config zig ];

  dontSetZigDefaultFlags = true;

  zigBuildFlags = [
    "--system"
    "${deps}"
    "-Dcpu=baseline"
    "-Doptimize=ReleaseFast"
  ];

  meta = {
    description = "Ghostty's VT (terminal sequence) parsing library";
    homepage = "https://ghostty.org/";
    license = lib.licenses.mit;
    pkgConfigModules = [ "libghostty-vt" "libghostty-vt-static" ];
    platforms = zig.meta.platforms;
  };
}
