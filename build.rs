use std::env;

use copy_to_output::copy_to_output;

fn main() {
    // Re-runs script if any files in res are changed
    println!("cargo:rerun-if-changed=resources\\core.ap");
    copy_to_output("resources", &env::var("PROFILE").unwrap()).expect("Could not copy");
}
