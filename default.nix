let
    ihp = builtins.fetchTarball {
        url = "https://ihp.digitallyinduced.com/BuildTarball?userId=f0a92fe8-8d53-4e8d-9aba-c96e37ce6573&token=okrAbQRohprwDJtzwdrogtOMVvyielRT&version=113aaeb26607c3b5a7fca79061cbf2a9af669105";
        sha256 = "1yqmi86i79q5gjkvdlbcr3h10nmn05x63qg10nnfd2ji4bq83sm9";
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
