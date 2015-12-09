filenames = FileNames["*", FileNameJoin[{$InitialDirectory, "tmp", "tweets"}]];
tweets = StringSplit[FindList[filenames, "knicks", IgnoreCase -> True], "\t"];
groupedTweets = Reverse[Sort[CountsBy[tweets, #[[2]] &]]];
Export[FileNameJoin[{$InitialDirectory, "tmp", "mathematica_output"}], List@@@Normal@groupedTweets, "TSV"];
