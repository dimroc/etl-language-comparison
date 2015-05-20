extern crate getopts;

use getopts::Options;
use std::env;

extern crate regex;

use std::fs::{File};
use std::io::prelude::*;
use std::io::{BufReader};
use std::sync::mpsc;
use std::thread;

use std::ascii::AsciiExt;
use std::collections::HashMap;

fn main() {
	let args: Vec<String> = env::args().collect();
	let program = args[0].clone();
	let mut options = Options::new();
	options.optflag("h", "help", "print this help menu");
	options.optflag("s", "substring", "use substring matching");
	let matches = options.parse(&args[1..]).unwrap();
	if matches.opt_present("h") {
		let brief = format!("Usage: {} [options]", program);
		print!("{}", options.usage(&brief));
		return;
	}
	let re = regex::Regex::new(r"(?i)knicks").unwrap();
	let substring = matches.opt_present("s");

	let (sender, receiver) = mpsc::channel();

	let paths = std::fs::read_dir("../tmp/tweets").unwrap();
	let mut n = 0;
	for path in paths {
		n += 1;
		let re = re.clone();
		let sender = sender.clone();
		thread::spawn(move || {
			let path = path.unwrap().path();
			let reader = BufReader::new(File::open(&path).unwrap());
			let mut map: HashMap<String, u64> = HashMap::new();
			for line in reader.lines() {
				let line = line.unwrap();
				let is_match = if substring {
					let line_lowercase = line.to_ascii_lowercase();
					line_lowercase.contains("knicks")
				} else {
					re.is_match(&line)
				};
				if is_match {
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

	let mut file = if substring {
		File::create("../tmp/rust_substring_output").unwrap()
	} else {
		File::create("../tmp/rust_regex_output").unwrap()
	};
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
