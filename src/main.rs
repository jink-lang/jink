use inkwell::context::Context;
use std::env;
use std::fs;
use std::fs::File;
use std::io::Write;

mod compiler;
mod interpreter;
use compiler::builder::CodeGen;
use compiler::parser::Parser;
use interpreter::simulator::Simulator;

fn main() {
  // Get args input
  let args: Vec<String> = env::args().collect();
  if args.len() < 2 || args.contains(&"-h".to_string()) {
    println!("Usage: jink <file.jk>");
    println!("Options:");
    println!("  -v: Verbose output");
    println!("  -i: Interpret code");
    println!("  -o: Output to an executable");
    println!("  -r: Run the executable after compilation");
    println!("  -l: Compile just to LLVM IR");
    println!("  -h: Display this help message");
    return;
  }

  if let Err(err) = fs::metadata(&args[1]) {
    println!("Error: {}", err);
    return;
  }

  // Collect flags
  let verbose = args.contains(&"-v".to_string());
  let interpret = args.contains(&"-i".to_string());
  let out = args.contains(&"-o".to_string());
  let run_after_out = args.contains(&"-r".to_string());
  let out_llvm = args.contains(&"-l".to_string());

  // Remove flags
  let args: Vec<String> = args.iter().map(|arg| arg.to_string()).filter(|arg| !arg.starts_with("-")).collect();

  let code = fs::read_to_string(&args[1])
    .expect("Failed to read file.");

  let mut parser = Parser::new();
  let parsed = parser.parse(
    code.clone(),
    fs::canonicalize(args[1].clone()).unwrap().to_str().unwrap().to_string(),
    verbose,
    false
  );

  // Verbose flag output to print namespace info
  if verbose {
    println!("------------------");
    println!("--- Namespaces ---");
    println!("------------------");
    for (namespace, ns) in parser.namespaces.iter() {
      if ns.names.is_empty() { continue; }
      println!("Namespace: {}", namespace);
      println!("  Names:");
      for (name, _) in ns.names.iter() {
        println!("   - {}", name);
      }
      println!("  Dependencies:");
      for (name, deps) in ns.dependencies.iter() {
        println!("   - {}: {:?}", name, deps);
      }
      println!(  "Import dependencies:");
      for (name, deps) in ns.imports.iter() {
        println!("   - {}: {:?}", name, deps);
      }
    }
  }

  if let Err(err) = parsed {
    println!("{}", err);
    return;
  }

  // if verbose {
  //   println!("AST:");
  //   println!("{:?}", parsed.as_ref().unwrap());
  // }

  if interpret {
    let mut simulator = Simulator::new();
    let simulated = simulator.simulate(code.clone(), parsed.unwrap(), verbose);
    if let Err(err) = simulated {
      println!("{}", err);
      return;
    }
    println!("{:?}", simulated);
  } else {
    let context = Context::create();
    let mut builder = CodeGen::new(&context);

    let do_execute = !out && !out_llvm;

    let main = builder.build(
      code.clone(),
      parsed.unwrap(),
      parser.namespaces,
      verbose,
      do_execute
    );
    if let Err(err) = main {
      println!("{}", err);
      return;
    }
    if do_execute { return; }

    // Put LLVM IR to file
    let mut file = File::create("output.ll").unwrap();
    file.write_all(builder.module.print_to_string().to_bytes()).unwrap();
    if out_llvm {
      println!("Outputted LLVM IR to output.ll");
      return;
    }

    // Run system command to compile the LLVM IR
    #[cfg(target_os = "windows")]
    let output = std::process::Command::new("clang")
      .arg("-o")
      .arg("output.exe")
      .arg("output.ll")
      .output()
      .expect("Failed to compile LLVM IR");
    #[cfg(not(target_os = "windows"))]
    let output = std::process::Command::new("clang")
      .arg("-o")
      .arg("output")
      .arg("output.ll")
      .output()
      .expect("Failed to compile LLVM IR");

    // Delete the LLVM IR file
    std::fs::remove_file("output.ll").unwrap();

    if !output.status.success() {
      println!("Error: {}", String::from_utf8_lossy(&output.stderr));
      return;
    }

    if run_after_out {
      let output = std::process::Command::new("./output")
        .output()
        .expect("Failed to run executable");

      if !output.status.success() && String::from_utf8_lossy(&output.stderr) != "" {
        println!("Error: {}", String::from_utf8_lossy(&output.stderr));
      } else {
        println!("{}", String::from_utf8_lossy(&output.stdout));
      }
    }
  }
}