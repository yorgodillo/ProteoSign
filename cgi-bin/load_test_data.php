 <?php
	//$test_data_dir = $_SERVER['DOCUMENT_ROOT'] . "/test data/";
	$test_data_dir = "C:/Users/G Efstathiou/Documents/GitHub/ProteoSign/test data/";
	$test_data_info_file = explode("\n", file_get_contents("$test_data_dir/info.txt"));
	//$descriptions_requested = ($_POST["description"] === "true");
	$descriptions_requested = true;
	$server_response = [];
	
	for ($i=0; $i < count($test_data_info_file); $i++)  
	{
		$line_i = $test_data_info_file[$i];
		$fields = explode(":", $line_i);
		if($descriptions_requested){
			$subfields = explode(" ", $fields[0]);
			$files = $subfields[1];
			$subfields = explode(" ", $fields[1]);
			$desc = $subfields[2];
			$server_response[] = $files . ":" . $desc;
		}else{
			/*
			for ($j=0; $j < count($fields); $j++){
				$subfields = explode(" ", $fields[$j]);
			}
			*/		
		}
	}	
 
 	header('Content-type: application/json');
	echo json_encode($server_response);	
?>