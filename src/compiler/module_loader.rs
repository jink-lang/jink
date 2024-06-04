use std::path::PathBuf;
use jink::Name;

/// Resolve module name to file path
// TODO: This will need to be updated when we flesh out a standard for modules
pub fn resolve_module_path(module: Vec<Name>) -> PathBuf {
  let mut path = PathBuf::from("src");
  for name in module {
    path.push(name.0);
  }
  path.set_extension("jk");
  return path;
}

pub fn load_module(module: Vec<Name>, verbose: bool) -> Option<String> {
  let path = resolve_module_path(module);
  let module = std::fs::read_to_string(path.clone());

  if let Err(err) = module {
    if verbose {
      println!("Failed to load module: {}", err.to_string());
    }
    return None
  } else {
    if verbose {
      println!("Loading module: {:?}", std::fs::canonicalize(path).unwrap().to_str().unwrap().to_string());
    }
    return Some(module.unwrap());
  }
}