<?php
	$session_folder = $_POST["session_id"];
	$document_root = dirname(__DIR__);
	$upload_dir = $document_root . "/uploads/" . $session_folder;
	$temp_path = "uploads/" . $session_folder;
	$experiment_name = $_POST["expname"];
	$my_text = $_POST["texttodownload"];
	$myfile = fopen($upload_dir . "/" .  $experiment_name . "_parameters.txt", "w");
	fwrite($myfile, $my_text);
	fclose($myfile);
	$server_response['success'] = true;
	$server_response['results_url'] = $temp_path . "/" .  $experiment_name . "_parameters.txt";
	header('Content-type: application/json');
echo json_encode($server_response);
?>