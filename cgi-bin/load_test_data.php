 <?php
	/*
	// DEBUG
	$test_data_dir = "C:/Users/G Efstathiou/Documents/GitHub/ProteoSign/test data/";
	$descriptions_requested = false;
	$dataset_requested = "dimethyl 2-plex (PD)";
	*/
	
	$test_data_dir = $_SERVER['DOCUMENT_ROOT'] . "/ProteoSign/test data/";
	$descriptions_requested = ($_POST["descriptions_requested"] === "true");
	
	$server_response = [];
	$server_response['success'] = false;
	$server_response['msg'] = "";
	$server_response['queryres'] = [];
	
	try{
		$db = new SQLite3("$test_data_dir/testdatadb");
	}
	catch (Exception $exception) {
		$server_response['msg'] = "Failed to connect to the database: " . $exception->getMessage();
		goto end;
	}

    if($descriptions_requested){
		$qres = $db->query('select desc from dataset');
	}else{
		$qres = $db->query('select selector,value from dataset inner join param_value on dataset.id=param_value.dataset_id inner join param on param_value.param_id=param.id where dataset.desc="' .  $_POST["dataset_info_requested"] . '"');
	}
	
	if(!$qres){
		$server_response['msg'] = "Failed to query the database: " . $sqlite->lastErrorMsg();
		goto end;		
	}
    while($qrow = $qres->fetchArray(SQLITE3_ASSOC)){
		foreach($qrow as $col => $val){
			$server_response['queryres'][$col][] = $val;
			//echo $col . ": " . $val . " ";
		}
		//echo "\n";
	}
	$qres->finalize();
	
	if(!$descriptions_requested){
		$qres = $db->query('select file from dataset inner join dataset_files on dataset.id=dataset_files.dataset_id inner join files on dataset_files.file_id = files.id where dataset.desc="' . $_POST["dataset_info_requested"] . '"');
		if(!$qres){
			$server_response['msg'] = "Failed to query the database: " . $sqlite->lastErrorMsg();
			goto end;		
		}
		while($qrow = $qres->fetchArray(SQLITE3_ASSOC)){
			foreach($qrow as $col => $val){
				$server_response['queryres'][$col][] = $val;
				//echo $col . ": " . $val . " ";
			}
			//echo "\n";
		}
		$qres->finalize();		
	}
	
	$db->close();
	$server_response['success'] = true;
	
	/*
	foreach($server_response['queryres']['selector'] as $key => $val){
		echo $key . ": " . $val . " ";
	}
	echo "\n";	
	foreach($server_response['queryres']['value'] as $key => $val){
		echo $key . ": " . $val . " ";
	}
	echo "\n";	
	foreach($server_response['queryres']['file'] as $key => $val){
		echo $key . ": " . $val . " ";
	}
	echo "\n";
	*/
 end:
 	header('Content-type: application/json');
	echo json_encode($server_response);	
?>