use std::process::Command;

use swipl_info::get_swipl_info;

fn main() {
  println!("cargo:rerun-if-changed=build.rs");
  println!("cargo:rerun-if-changed=mlatu.pl");
  println!("cargo:rerun-if-changed=mlatu.pl.save");
  println!("cargo:rustc-link-arg=-Wl,-rpath,{}", get_swipl_info().lib_dir);
  Command::new("swipl").args(["-o", "mlatu.pl.save", "--goal=true", "-c", "mlatu.pl"])
                       .spawn()
                       .expect("spawn")
                       .wait()
                       .expect("wait");
}
