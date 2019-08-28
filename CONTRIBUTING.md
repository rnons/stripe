To setup your development environment, you'll need the [Nix package manager](https://nixos.org/nix/).

1. Clone the repo
   ```
   git clone git@github.com:rnons/stripe.git
   cd stripe
   ```
2. Develop stripe-core
   ```
   nix-shell
   ghcid -c 'cabal new-repl stripe-core'
   ```
3. Develop stripe-http-client
   ```
   nix-shell
   ghcid -c 'cabal new-repl stripe-http-client'
   ```
4. Develop stripe-tests
   ```
   nix-shell
   ghcid -c 'cabal new-repl stripe-tests'
   ```
5. Run tests

   Start [stripe-mock](https://github.com/stripe/stripe-mock), then
   ```
   nix-shell
   STRIPEKEY=sk_test_1234 cabal new-test stripe-http-client
   ```
