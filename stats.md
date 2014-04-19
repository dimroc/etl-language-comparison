# Golang

### Mapping one file of 12M tweets (1.2GB)

`time $(go run golang/src/map.go tmp/all_tweets output > golang_all_mapped)`

real  3m23.602s
user  2m55.514s
sys   0m28.419s


### Mapping thirteen files of ~100K tweets sequentially

`time go run src/map.go ../tmp/tweets output`

real  3m29.893s
user  2m58.450s
sys   0m27.758s

### Mapping thirteen files of ~100K tweets concurrently

`time go run src/map.go ../tmp/tweets output`

real  3m22.682s
user  2m56.235s
sys   0m26.472s

### Mapping thirteen files of ~100K tweets concurrently, and reduction

real  3m23.165s
user  2m56.002s
sys   0m26.542s
