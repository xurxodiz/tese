<?php

header ("content-type: application/xml");

echo '<?xml version="1.0" encoding="UTF-8"?>';

function socket_get($ip, $port, $data) {
  $output = "";

  // Create a TCP Stream Socket
  $socket = socket_create(AF_INET, SOCK_STREAM, SOL_TCP);
  if ($socket === false)
    throw new Exception("Socket Creation Failed");

  // Connect to the server.
  $result = socket_connect($socket, $ip, $port);
  if ($result === false)
    throw new Exception("Connection Failed");

  // Write to socket!
  socket_write($socket, utf8_decode($data), strlen($data));

  // Read from socket!
  do {
    $line = @socket_read($socket, 1024, PHP_NORMAL_READ);
    $output .= $line;
  } while ($line != "");

  // Close and return.
  socket_close($socket);
  return utf8_encode($output);
}

function addlog($q,$r)
{ 
  date_default_timezone_set ('Europe/Madrid');
  $str = "[" . date("Y/m/d h:i:s", mktime()) . "] {$_SERVER['REMOTE_ADDR']}\n";
  $str .= $q . $r . "\n"; 
  $fd = fopen("log.log", "a");
  fwrite($fd, $str);
  fclose($fd);
}

$q = trim($_POST['question']) . "\n";

$output = socket_get("localhost", "3838", $q);

addlog($q,$output);

echo '<answer>'. htmlspecialchars($output) . '</answer>';

?>
