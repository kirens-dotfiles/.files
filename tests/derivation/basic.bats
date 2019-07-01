#! /usr/bin/env bats

@test "A laptop derivation exists" {
  test -x result/laptop
}
