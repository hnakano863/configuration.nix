# This file has been generated by node2nix 1.11.1. Do not edit!

{nodeEnv, fetchurl, fetchgit, nix-gitignore, stdenv, lib, globalBuildInputs ? []}:

let
  sources = {
    "argparse-2.0.1" = {
      name = "argparse";
      packageName = "argparse";
      version = "2.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/argparse/-/argparse-2.0.1.tgz";
        sha512 = "8+9WqebbFzpX9OR+Wa6O29asIogeRMzcGtAINdpMHHyAg10f05aSFVBbcEqGf/PXw1EjAZ+q2/bEBg3DvurK3Q==";
      };
    };
    "balanced-match-1.0.2" = {
      name = "balanced-match";
      packageName = "balanced-match";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.2.tgz";
        sha512 = "3oSeUO0TMV67hN1AmbXsK4yaqU7tjiHlbxRDZOpH0KW9+CeX4bRAaX0Anxt0tx2MrpRpWwQaPwIlISEJhYU5Pw==";
      };
    };
    "bluebird-3.7.2" = {
      name = "bluebird";
      packageName = "bluebird";
      version = "3.7.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/bluebird/-/bluebird-3.7.2.tgz";
        sha512 = "XpNj6GDQzdfW+r2Wnn7xiSAd7TM3jzkxGXBGTtWKuSXv1xUV+azxAm8jdWZN06QTQk+2N2XB9jRDkvbmQmcRtg==";
      };
    };
    "brace-expansion-1.1.11" = {
      name = "brace-expansion";
      packageName = "brace-expansion";
      version = "1.1.11";
      src = fetchurl {
        url = "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz";
        sha512 = "iCuPHDFgrHX7H2vEI/5xpz07zSHB00TpugqhmYtVmMO6518mCuRMoOYFldEBl0g187ufozdaHgWKcYFb61qGiA==";
      };
    };
    "concat-map-0.0.1" = {
      name = "concat-map";
      packageName = "concat-map";
      version = "0.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz";
        sha512 = "/Srv4dswyQNBfohGpz9o6Yb3Gz3SrUDqBH5rTuhGR7ahtlbYKnVxw2bCFMRljaA7EXHaXZ8wsHdodFvbkhKmqg==";
      };
    };
    "dot-1.1.3" = {
      name = "dot";
      packageName = "dot";
      version = "1.1.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/dot/-/dot-1.1.3.tgz";
        sha512 = "/nt74Rm+PcfnirXGEdhZleTwGC2LMnuKTeeTIlI82xb5loBBoXNYzr2ezCroPSMtilK8EZIfcNZwOcHN+ib1Lg==";
      };
    };
    "fromentries-1.3.2" = {
      name = "fromentries";
      packageName = "fromentries";
      version = "1.3.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/fromentries/-/fromentries-1.3.2.tgz";
        sha512 = "cHEpEQHUg0f8XdtZCc2ZAhrHzKzT0MrFUTcvx+hfxYu7rGMDc5SKoXFh+n4YigxsHXRzc6OrCshdR1bWH6HHyg==";
      };
    };
    "fs.realpath-1.0.0" = {
      name = "fs.realpath";
      packageName = "fs.realpath";
      version = "1.0.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz";
        sha512 = "OO0pH2lK6a0hZnAdau5ItzHPI6pUlvI7jMVnxUQRtw4owF2wk8lOSabtGDCTP4Ggrg2MbGnWO9X8K1t4+fGMDw==";
      };
    };
    "glob-7.2.3" = {
      name = "glob";
      packageName = "glob";
      version = "7.2.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/glob/-/glob-7.2.3.tgz";
        sha512 = "nFR0zLpU2YCaRxwoCJvL6UvCH2JFyFVIvwTLsIf21AuHlMskA1hhTdk+LlYJtOlYt9v6dvszD2BGRqBL+iQK9Q==";
      };
    };
    "inflight-1.0.6" = {
      name = "inflight";
      packageName = "inflight";
      version = "1.0.6";
      src = fetchurl {
        url = "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz";
        sha512 = "k92I/b08q4wvFscXCLvqfsHCrjrF7yiXsQuIVvVE7N82W3+aqpzuUdBbfhWcy/FZR3/4IgflMgKLOsvPDrGCJA==";
      };
    };
    "inherits-2.0.4" = {
      name = "inherits";
      packageName = "inherits";
      version = "2.0.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/inherits/-/inherits-2.0.4.tgz";
        sha512 = "k/vGaX4/Yla3WzyMCvTQOXYeIHvqOKtnqBduzTHpzpQZzAskKMhZ2K+EnBiSM9zGSoIFeMpXKxa4dYeZIQqewQ==";
      };
    };
    "js-yaml-4.1.0" = {
      name = "js-yaml";
      packageName = "js-yaml";
      version = "4.1.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/js-yaml/-/js-yaml-4.1.0.tgz";
        sha512 = "wpxZs9NoxZaJESJGIZTyDEaYpl0FKSA+FB9aJiyemKhMwkxQg63h4T1KJgUGHpTqPDNRcmmYLugrRjJlBtWvRA==";
      };
    };
    "jsonpath-plus-7.2.0" = {
      name = "jsonpath-plus";
      packageName = "jsonpath-plus";
      version = "7.2.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/jsonpath-plus/-/jsonpath-plus-7.2.0.tgz";
        sha512 = "zBfiUPM5nD0YZSBT/o/fbCUlCcepMIdP0CJZxM1+KgA4f2T206f6VAg9e7mX35+KlMaIc5qXW34f3BnwJ3w+RA==";
      };
    };
    "liyad-0.2.4" = {
      name = "liyad";
      packageName = "liyad";
      version = "0.2.4";
      src = fetchurl {
        url = "https://registry.npmjs.org/liyad/-/liyad-0.2.4.tgz";
        sha512 = "490GSJDZ/K7Paxflg3naejlmsPri6Af4+FgXlcQiktY0TSQjj+QBu0qP3pKRFGGjOwT7CTK5CTEoanAyzbq+Wg==";
      };
    };
    "lookml-parser-6.10.0" = {
      name = "lookml-parser";
      packageName = "lookml-parser";
      version = "6.10.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/lookml-parser/-/lookml-parser-6.10.0.tgz";
        sha512 = "eA8+zt7wUKXnkx+7M86Z7mRqta3+TZhMkYTVmyMau+F7a5SzjMhsXzQqWmQgyZmD/iRElWyb+6LQ2b/5SSwBaQ==";
      };
    };
    "minimatch-3.1.2" = {
      name = "minimatch";
      packageName = "minimatch";
      version = "3.1.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/minimatch/-/minimatch-3.1.2.tgz";
        sha512 = "J7p63hRiAjw1NDEww1W7i37+ByIrOWO5XQQAzZ3VOcL0PNybwpfmV/N05zFAzwQ9USyEcX6t3UO+K5aqBQOIHw==";
      };
    };
    "minimist-1.2.8" = {
      name = "minimist";
      packageName = "minimist";
      version = "1.2.8";
      src = fetchurl {
        url = "https://registry.npmjs.org/minimist/-/minimist-1.2.8.tgz";
        sha512 = "2yyAR8qBkN3YuheJanUpWC5U3bb5osDywNB8RzDVlDwDHbocAJveqqj1u8+SVD7jkWT4yvsHCpWqqWqAxb0zCA==";
      };
    };
    "once-1.4.0" = {
      name = "once";
      packageName = "once";
      version = "1.4.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/once/-/once-1.4.0.tgz";
        sha512 = "lNaJgI+2Q5URQBkccEKHTQOPaXdUxnZZElQTZY0MFUAuaEqe1E+Nyvgdz/aIyNi6Z9MzO5dv1H8n58/GELp3+w==";
      };
    };
    "path-is-absolute-1.0.1" = {
      name = "path-is-absolute";
      packageName = "path-is-absolute";
      version = "1.0.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz";
        sha512 = "AVbw3UJ2e9bq64vSaS9Am0fje1Pa8pbGqTTsmXfaIiMpnr5DlDhfJOuLj9Sf95ZPVDAUerDfEk88MPmPe7UCQg==";
      };
    };
    "pegjs-0.10.0" = {
      name = "pegjs";
      packageName = "pegjs";
      version = "0.10.0";
      src = fetchurl {
        url = "https://registry.npmjs.org/pegjs/-/pegjs-0.10.0.tgz";
        sha512 = "qI5+oFNEGi3L5HAxDwN2LA4Gg7irF70Zs25edhjld9QemOgp0CbvMtbFcMvFtEo1OityPrcCzkQFB8JP/hxgow==";
      };
    };
    "require-from-string-2.0.2" = {
      name = "require-from-string";
      packageName = "require-from-string";
      version = "2.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/require-from-string/-/require-from-string-2.0.2.tgz";
        sha512 = "Xf0nWe6RseziFMu+Ap9biiUbmplq6S9/p+7w7YXP/JBHhrUDDUhwa+vANyubuqfZWTveU//DYVGsDG7RKL/vEw==";
      };
    };
    "wrappy-1.0.2" = {
      name = "wrappy";
      packageName = "wrappy";
      version = "1.0.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz";
        sha512 = "l4Sp/DRseor9wL6EvV2+TuQn63dMkPjZ/sp9XkghTEbV9KlPS1xUsZ3u7/IQO4wxtcFB4bgpQPRcR3QCvezPcQ==";
      };
    };
  };
  args = {
    name = "_at_looker_slash_look-at-me-sideways";
    packageName = "@looker/look-at-me-sideways";
    version = "3.4.0";
    src = fetchurl {
      url = "https://registry.npmjs.org/@looker/look-at-me-sideways/-/look-at-me-sideways-3.4.0.tgz";
      sha512 = "W+tqePJWNdCthjXVqQHw0MpAqZXXfu7CRCkMy3n5b2OKGr1p4S6Y99xS217sx9kFs06TBve2EzqoLTfHWM9gcg==";
    };
    dependencies = [
      sources."argparse-2.0.1"
      sources."balanced-match-1.0.2"
      sources."bluebird-3.7.2"
      sources."brace-expansion-1.1.11"
      sources."concat-map-0.0.1"
      sources."dot-1.1.3"
      sources."fromentries-1.3.2"
      sources."fs.realpath-1.0.0"
      sources."glob-7.2.3"
      sources."inflight-1.0.6"
      sources."inherits-2.0.4"
      sources."js-yaml-4.1.0"
      sources."jsonpath-plus-7.2.0"
      sources."liyad-0.2.4"
      sources."lookml-parser-6.10.0"
      sources."minimatch-3.1.2"
      sources."minimist-1.2.8"
      sources."once-1.4.0"
      sources."path-is-absolute-1.0.1"
      sources."pegjs-0.10.0"
      sources."require-from-string-2.0.2"
      sources."wrappy-1.0.2"
    ];
    buildInputs = globalBuildInputs;
    meta = {
      description = "A judgmental little LookML linter >_>";
      homepage = "https://github.com/looker-open-source/look-at-me-sideways#readme";
      license = "MIT";
    };
    production = true;
    bypassCache = true;
    reconstructLock = true;
  };
in
{
  args = args;
  sources = sources;
  tarball = nodeEnv.buildNodeSourceDist args;
  package = nodeEnv.buildNodePackage args;
  shell = nodeEnv.buildNodeShell args;
  nodeDependencies = nodeEnv.buildNodeDependencies (lib.overrideExisting args {
    src = stdenv.mkDerivation {
      name = args.name + "-package-json";
      src = nix-gitignore.gitignoreSourcePure [
        "*"
        "!package.json"
        "!package-lock.json"
      ] args.src;
      dontBuild = true;
      installPhase = "mkdir -p $out; cp -r ./* $out;";
    };
  });
}
