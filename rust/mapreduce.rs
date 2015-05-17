use std::fs::{File};
use std::io::prelude::*;
use std::io::{BufReader};
use std::sync::mpsc;
use std::thread;

use std::ascii::AsciiExt;
use std::collections::HashMap;

fn main() {
	let (sender, receiver) = mpsc::channel();

	let paths = std::fs::read_dir("../tmp/tweets").unwrap();
	let mut n = 0;
	for path in paths {
		n += 1;
		let sender = sender.clone();
		thread::spawn(move || {
			let path = path.unwrap().path();
			let reader = BufReader::new(File::open(&path).unwrap());
			let mut map: HashMap<String, u64> = HashMap::new();
			for line in reader.lines() {
				let line = line.unwrap();
				let line_lowercase = line.to_ascii_lowercase();
				if line_lowercase.contains("knicks") {
					let hood = line.split("\t").nth(1).unwrap();
					let counter = map.entry(hood.to_string()).or_insert(0);
					*counter += 1;
				}
			}
			sender.send(map).unwrap();
		});
	}

	let mut primary_map: HashMap<String, u64> = HashMap::new();
	for _ in 0..n {
		let secondary_map = receiver.recv().unwrap();
		for (key, value) in secondary_map.iter() {
			let counter = primary_map.entry(key.to_string()).or_insert(0);
			*counter += *value;
		}
	}

	let mut inverted_map: HashMap<u64, Vec<String>> = HashMap::new();
	for (key, value) in primary_map.iter() {
		let v = inverted_map.entry(*value).or_insert(Vec::new());
		v.push(key.to_string());
	}

	let mut values: Vec<u64> = Vec::new();
	for value in inverted_map.keys() {
		values.push(*value);
	}
	values.sort();
	values.reverse();

	let mut file = File::create("../tmp/rust_output").unwrap();
	for value in values.iter() {
		let keys = inverted_map.get(value).unwrap();
		let mut hoods: Vec<String> = Vec::new();
		for key in keys.iter() {
			hoods.push(key.to_string());
		}
		hoods.sort();
		for hood in hoods.iter() {
			writeln!(&mut file, "{}\t{:?}", hood, value).unwrap();
		}
	}
}
