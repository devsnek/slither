workflow "On Push" {
  on = "push"
  resolves = ["check"]
}

workflow "On PR" {
  on = "pull_request"
  resolves = ["check"]
}

action "check" {
  uses = "icepuma/rust-action@master"
  args = "cargo fmt -- --check && cargo clippy -- -Dwarnings && cargo test"
}
