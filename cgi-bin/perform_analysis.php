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
	$document_root = $_SERVER['DOCUMENT_ROOT'] . "/ProteoSign";
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
   $cgibin_dir = getcwd();
   chdir($upload_dir);
   mkdir('msdiffexp_wd');
   copy($cgibin_dir . '/MSdiffexp.R', $upload_dir . '/msdiffexp_wd/MSdiffexp.R');
   rename('MSdiffexp_definitions.R', 'msdiffexp_wd/MSdiffexp_definitions.R');
   rename('exp_struct.txt', 'msdiffexp_wd/exp_struct.txt');
   rename('msdiffexp_peptide.txt', 'msdiffexp_wd/msdiffexp_peptide.txt');
   rename('msdiffexp_protein.txt', 'msdiffexp_wd/msdiffexp_protein.txt');
   chdir('msdiffexp_wd');
   // Keep waiting, for longer and longer time periods if there isn't enough memory (empirically determined to be at least double the size of the PSMs file) to do the analysis
   $free_mem_kB = 0;
   $sleep_time_secs = 0;
   $free_mem_required_kB = filesize('msdiffexp_wd/msdiffexp_peptide.txt') * 0.002;
   do{
      sleep($sleep_time_secs++);
      $tmp = [];
      exec("cat /proc/meminfo | grep 'MemFree:' | sed -r 's/.*\s([0-9]+)\s.*/\1/'", $tmp);
      $free_mem_kB = intval($tmp[0]);
   } while($free_mem_kB < $free_mem_required_kB);
   
   exec('R CMD BATCH --slave MSdiffexp.R msdiffexp_log.txt');
	// Determine success of R run by search for 'error' occurrences in msdiffexp_log.txt
	$R_logfile = file_get_contents("msdiffexp_log.txt");
	if($R_logfile){
		$r_logfile_error_lines = [];
		if(preg_match_all("/Error (.*)/s", $R_logfile, $r_logfile_error_lines) > 0){
			$server_response['R_dump'] = implode(" -*- Error -*-\n", array_values($r_logfile_error_lines[1]));
			$server_response['msg'] = "The R script returned error(s).";
			$server_response['dump'] = $server_response['R_dump'];
		}else{
			$server_response['R_success'] = true;
		}
	}
   rename('msdiffexp_log.txt', 'msdiffexp_out/log.txt');
   chdir($upload_dir);
	if($server_response['R_success']){
      exec("cp msdiffexp_wd/msdiffexp_out/*.pdf .; mogrify -format png -density 150 -quality 100 -fill white -opaque none *.pdf; rm *.pdf");
      exec("zip -j proteosign.zip msdiffexp_wd/msdiffexp_out/*");
   }
	
	// Determine success of analysis
	$server_response['success'] = $server_response['R_success'];
	if($server_response['success']){
		//$server_response['results_url'] = $upload_dir . "/msdiffexp.zip";
		// NOT LOCALHOST DEPLOYMENT VERSION
		$server_response['results_url'] = str_replace($_SERVER['DOCUMENT_ROOT'], '', $upload_dir . "/proteosign.zip");
		//
		$server_response['results_preview'] = [];
		$pngs = glob("*.png");
		// LOCALHOST DEPLOYMENT VERSION ONLY
		//$server_response['results_preview'] = $dirs;
		//
		// NOT LOCALHOST DEPLOYMENT VERSION
		foreach($pngs as $png_i){
			$server_response['results_preview'][] = str_replace($_SERVER['DOCUMENT_ROOT'], '', $upload_dir . "/" . $png_i);
		}
		//
	}
end:
	error_log("perform_analysis.php [" . $_POST["session_id"] . "]> Success: " . ($server_response['success'] ? 'Yes' : 'No') . " | Message: " . $server_response['msg']);
	if(!$server_response['success']){
		error_log("perform_analysis.php [" . $_POST["session_id"] . "]> Relevant dump: " . $server_response['dump']);
	}else{
      exec('rm -Rf msdiffexp_wd');
   }
	//Send info back to the client
	//error_log(json_encode($server_response));
	header('Content-type: application/json');
	echo json_encode($server_response);	
?>
        
       