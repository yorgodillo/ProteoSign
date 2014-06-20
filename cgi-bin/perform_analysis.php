 <?php
	$server_response = [];
	$server_response['success'] = false;
	$server_response['msg'] = "";
	$server_response['dispatcher_success'] = false;
	$server_response['dispatcher_dump'] = [];
	$server_response['R_success'] = false;
	$server_response['R_dump'] = "";
	$server_response['dump'] = "";
	
	$session_folder = $_POST["session_id"];
	$document_root = $_SERVER['DOCUMENT_ROOT'];
	$upload_dir = $document_root . "/uploads/" . $session_folder;
	// clean files from possible previous run
	$pngs = glob($upload_dir . "/*.png");
	foreach($pngs as $png){
		if(is_file($png)){
			unlink($png);
		}
	}
	if(is_file($upload_dir . "/msdiffexp.zip")){
		unlink($upload_dir . "/msdiffexp.zip");
	}
	// Run analysis
	exec("perl proteosign_dispatcher.pl \"$upload_dir/parameters.txt\" 2>&1", $server_response['dispatcher_dump']);
	// Determine success of dispatcher run by counting the number of "DONE" + newline character
	$server_response['dispatcher_success'] = (count($server_response['dispatcher_dump']) == 6);
	$server_response['dispatcher_dump'] = implode("\n", $server_response['dispatcher_dump']);
	if(!$server_response['dispatcher_success']){
		$server_response['msg'] = "The dispatcher returned error(s).";
		$server_response['dump'] = $server_response['dispatcher_dump'];
		goto end;
	}
	// Determine success of R run by search for 'error' occurrences in msdiffexp_log.txt
	$R_logfile = file_get_contents("$upload_dir/msdiffexp_log.txt");
	if($R_logfile){
		$r_logfile_error_lines = [];
		if(preg_match_all("/Error (.*)/s", $R_logfile, $r_logfile_error_lines) > 0){
			$server_response['R_dump'] = implode(" -*- Error -*-\n", array_values($r_logfile_error_lines[1]));
			$server_response['msg'] = "The R script returned error(s).";
			$server_response['dump'] = $server_response['R_dump'];
		}else{
			$server_response['R_success'] = true;
		}
		unlink("$upload_dir/msdiffexp_log.txt");
	}else{
		$server_response['msg'] = "The file $upload_dir/msdiffexp_log.txt could not be read ('file_get_contents' returned FALSE)";
	}
	
	
	// Determine success of analysis
	$server_response['success'] = $server_response['dispatcher_success'] && $server_response['R_success'];
	if($server_response['success']){
		//$server_response['results_url'] = $upload_dir . "/msdiffexp.zip";
		// NOT LOCALHOST DEPLOYMENT VERSION
		$server_response['results_url'] = str_replace($_SERVER['DOCUMENT_ROOT'], '', $upload_dir . "/msdiffexp.zip");
		//
		$server_response['results_preview'] = [];
		$dirs = glob($upload_dir . "/*.png");
		// LOCALHOST DEPLOYMENT VERSION ONLY
		//$server_response['results_preview'] = $dirs;
		//
		// NOT LOCALHOST DEPLOYMENT VERSION
		foreach($dirs as $dir_i){
			$server_response['results_preview'][] = str_replace($_SERVER['DOCUMENT_ROOT'], '', $dir_i);
		}
		//
	}
end:
	error_log("perform_analysis.php [" . $_POST["session_id"] . "]> Success: " . ($server_response['success'] ? 'Yes' : 'No') . " | Message: " . $server_response['msg']);
	if(!$server_response['success']){
		error_log("perform_analysis.php [" . $_POST["session_id"] . "]> Relevant dump: " . $server_response['dump']);
	}
	//Send info back to the client
	//error_log(json_encode($server_response));
	header('Content-type: application/json');
	echo json_encode($server_response);	
?>
        
       