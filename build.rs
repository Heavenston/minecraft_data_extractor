use std::fs::File;
use std::io::Read;
use std::path::Path;
use walkdir::WalkDir;
use sha2::{Sha256, Digest};

fn main() {
    println!("cargo:rerun-if-changed=src");
    println!("cargo:rerun-if-changed=Cargo.toml");
    println!("cargo:rerun-if-changed=Cargo.lock");

    // Hashes all source files into an env variable
    println!("cargo:rustc-env=CODE_HASH={}", compute_code_hash());
}

fn compute_code_hash() -> String {
    let mut hasher = Sha256::new();

    let paths_to_hash = vec!["src", "Cargo.toml", "Cargo.lock"];

    for root_path in paths_to_hash {
        let path = Path::new(root_path);
        
        if path.is_dir() {
            let walker = WalkDir::new(path).sort_by_file_name();
            
            for entry in walker {
                let entry = entry.expect("Failed to read directory entry");
                let path = entry.path();
                
                if path.is_file() {
                    hash_file(path, &mut hasher);
                }
            }
        } else if path.is_file() {
            hash_file(path, &mut hasher);
        }
    }

    hex::encode(hasher.finalize())
}

fn hash_file(path: &Path, hasher: &mut Sha256) {
    let mut file = File::open(path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");
    
    hasher.update(path.to_string_lossy().as_bytes());
    hasher.update(&buffer);
}
