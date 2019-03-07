workflow "Quickstart" {
  on = "push"
  resolves = ["check"]
}

action "check" {
  uses = "icepuma/rust-action@master"
  args = "cargo fmt -- --check && cargo clippy -- -Dwarnings && cargo test"
}
