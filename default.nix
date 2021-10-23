let
    ihp = builtins.fetchTarball {
        url = "https://ihp.digitallyinduced.com/BuildTarball?userId=f0a92fe8-8d53-4e8d-9aba-c96e37ce6573&token=okrAbQRohprwDJtzwdrogtOMVvyielRT&version=228b209709c497a5892a798a44c98699a6e75054";
        sha256 = "0ravva9vhdw7bp3a55a6b08jwvyz9mpgimxxx4cb67d8m7bv2amx";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            p.ihp
            tz
            string-interpolate
            ihp-oauth-google
            ihp-sentry
        ];
        otherDeps = p: with p; [
            imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
