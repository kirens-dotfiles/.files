#! /usr/bin/env bats

@test "Building without specifying attribute results in an error" {
  code=0
  timeout -k 1 1 nix-build || code="$?"
  test "$code" != 124 # timeout - probably building something
  test "$code" != 0 # success - probably not sending an error
}
