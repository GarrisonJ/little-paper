let
    ihp = builtins.fetchTarball {
        url = "https://ihp.digitallyinduced.com/BuildTarball?userId=f0a92fe8-8d53-4e8d-9aba-c96e37ce6573&token=okrAbQRohprwDJtzwdrogtOMVvyielRT&version=e378330edf00bdfd804d23db05b9bc7756dffd3e";
        sha256 = "0q1w7kclyr73a9m7scxsa5k5jl3a08q122fxi91rgsbczaq227l2";
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
