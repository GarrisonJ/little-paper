let
    ihp = builtins.fetchTarball {
        url = "https://ihp.digitallyinduced.com/BuildTarball?userId=f0a92fe8-8d53-4e8d-9aba-c96e37ce6573&token=okrAbQRohprwDJtzwdrogtOMVvyielRT&version=c1a83966083cc429f0d218bd3a43d539053af8d9";
        sha256 = "06i6j6k1dbxl3329r1mz2bvrji16a9bazj3m8c82kg4c09q20ik4";
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
        ];
        otherDeps = p: with p; [
            imagemagick
        ];
        projectPath = ./.;
    };
in
    haskellEnv
