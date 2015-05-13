 <?php
	$server_response = [];
	$server_response['success'] = false;
	$server_response['msg'] = "";
	
	$session_folder = $_POST["session_id"];
	$document_root = $_SERVER['DOCUMENT_ROOT'] . "/ProteoSign";
	$upload_dir = $document_root . "/uploads/" . $session_folder;
	$upload_parameter_file = $upload_dir . "/MSdiffexp_definitions.R";
	$upload_experimental_structure_file = $upload_dir . "/exp_struct.txt";
	$parameters_template = "parameters_template.R";

	$the_parameters["REPLACE1"] = $_POST["exppddata"];
	$the_parameters["REPLACE2"] = "\"" . $_POST["expid"] . "\"";
	$the_parameters["REPLACE3"] = "\"" . $_POST["exptpoint"] . "\"";
	$the_parameters["REPLACE4"] = $_POST["expprotquant"];
	$the_parameters["REPLACE5"] = $_POST["expquantfilt"];
	$the_parameters["REPLACE6"] = $_POST["expquantfiltprot"];
	$the_parameters["REPLACE7"] = "\"" . $_POST["expquantfiltlbl"] . "\"";
	$the_parameters["REPLACE8"] = $_POST["labelfree"];
   $the_parameters["REPLACE9"] = "\"" . $upload_dir . '/msdiffexp_wd' . "\"";
	if($_POST["explbl00"] == "T" && isset($_POST["explbl0"]) && strlen($_POST["explbl0"]) > 0){
		$the_parameters["APPEND0"] = "Label\t" . $_POST["explbl0"] . "\t";
	}
	$lbl_i = 1;
	while(isset($_POST["explbl" . $lbl_i . "name"])){
		//$the_parameters["APPEND" . $lbl_i] = "Label\t" . $_POST["explbl" . $lbl_i . "name"] . "\t" . $_POST["explbl" . $lbl_i . "def"];
      $tmp = explode(",", $_POST["explbl" . $lbl_i . "def"]);
      $tmp = implode("\",\"", $tmp);
      $the_parameters["APPEND" . $lbl_i] = "addLabel(\"" . $_POST["explbl" . $lbl_i . "name"] . "\",c(\"" . $tmp . "\"))";
		$lbl_i++;
	}
	$parameters_file_contents = file_get_contents("$parameters_template") ;
	if($parameters_file_contents){
		while (list($key, $param_value) = each($the_parameters)) {
			if(preg_match('/^REPLACE/',$key)){
				$parameters_file_contents = preg_replace('/' . $key . "([[:space:]])" .  '/', $param_value . "$1", $parameters_file_contents);
			}else if(preg_match('/^APPEND/',$key)){
				$parameters_file_contents = $parameters_file_contents . $param_value . PHP_EOL;
			}
		}
	}else{
		$server_response['msg'] = "The file $parameters_template could not be read ('file_get_contents' returned FALSE)";
	}

	if($ff = fopen($upload_parameter_file, 'w')){
		$canwrite = fwrite($ff, $parameters_file_contents);
		if(!$canwrite){
			$server_response['msg'] = "The file $upload_parameter_file could not be written ('fwrite' returned FALSE)";
		}
		if($canwrite && !fclose($ff)){
			$server_response['msg'] = "The file $upload_parameter_file could not be closed ('fclose' returned FALSE)";
		}else{
			$server_response['success'] = true;
		}
	}else{
		$server_response['msg'] = "The file $upload_parameter_file could not be opened ('fopen' returned FALSE)";
	}
	
	if($server_response['success']){
		$server_response['success'] = false;
		if($ff = fopen($upload_experimental_structure_file, 'w')){
			$canwrite = fwrite($ff, $_POST["exp_struct"]);
			if(!$canwrite){
				$server_response['msg'] = "The file $upload_experimental_structure_file could not be written ('fwrite' returned FALSE)";
			}
			if($canwrite && !fclose($ff)){
				$server_response['msg'] = "The file $upload_experimental_structure_file could not be closed ('fclose' returned FALSE)";
			}else{
				$server_response['success'] = true;
			}
		}else{
			$server_response['msg'] = "The file $upload_experimental_structure_file could not be opened ('fopen' returned FALSE)";
		}	
	}
end:
	error_log("upload_parameters.php [" . $_POST["session_id"] . "]> Success: " . ($server_response['success'] ? 'Yes' : 'No') . " | Message: " . $server_response['msg']);
	//Send info back to the client
	header('Content-type: application/json');
	echo json_encode($server_response);	
?>
        
       