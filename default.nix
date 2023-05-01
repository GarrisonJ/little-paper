let
    ihp = builtins.fetchTarball {
        url = "https://ihp.digitallyinduced.com/BuildTarball?userId=f0a92fe8-8d53-4e8d-9aba-c96e37ce6573&token=okrAbQRohprwDJtzwdrogtOMVvyielRT&version=8bb12a20546c2b9ac899471cf7638147b9c90f47";
        sha256 = "0mg75hcw6pp59z5rq067y2i5n6xvrhy3spnh7g1hllkvy2s05f4s";
    };
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            cabal-install
            base
            wai
            text
            hlint
            xss-sanitize
            p.ihp
            tz
            string-interpolate
            ihp-oauth-google
            ihp-sentry
            JuicyPixels
        ];
        otherDeps = p: with p; [
            imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
