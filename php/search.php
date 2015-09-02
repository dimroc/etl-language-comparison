<?php
class Hash {
    public function __get($key)
    {
        $this->$key = 0;
        return 0;
    }
}
$result = new Hash();
$dir = __DIR__ . '/../tmp/tweets';
foreach(scandir($dir) as $file) {
    if ($file === '.' || $file === '..') {
        continue;
    }

    $fh = fopen($dir . '/' . $file, 'r');
    while($line = fgets($fh)) {
        if (stripos($line, 'knicks') !== false) {
            list($_, $hood) = explode("\t", $line, 3);
            $result->$hood++;
        }
    }
    fclose($fh);
}
$result = (array)$result;
arsort($result);
$fh = fopen($dir . '/../php_output', 'w');
foreach($result as $hood => $count) {
    fwrite($fh, $hood);
    fwrite($fh, "\t");
    fwrite($fh, $count);
    fwrite($fh, "\n");
}
fclose($fh);