<?php
	require 'get_labels.php';
	//"get_labels" function arguments for different file formats
	$get_labels_aguments_sets = array(
	   array('/^([^\s]+)\/([^\s]+)$/','/Modifications/','/\((?:[^:]+?:)?(.+?)\)(;|$)/'),	//Proteome Discoverer
	   array('/^Ratio ([^\s]+)\/([^\s]+)/',null,null),	//MaxQuant
	   array(null,'/Spectrum File/',null),	// PD label-free
	   array(null,'/Raw file/',null)	//MaxQuant label-free
	);
	
	$get_labels_aguments_sets_labelfree_flag = [false, false, true, true];
	
	$name = $_FILES['thefile']['name'];
	$cleanprev = ($_POST['cleanprev'] === 'true');
	$tmp_name = $_FILES['thefile']['tmp_name'];
	$error = $_FILES['thefile']['error'];
	$server_response = [];
	//Overall success, will be checked by client.
	$server_response['success'] = false;
	$server_response['msg'] = "";
	$server_response['peptide_labels'] = [];
	$server_response['peptide_labels_names'] = [];
	$server_response['skipped_labels'] = [];
	if (isset ($name)) {
		if (!empty($name)) {
			$location = $_SERVER['DOCUMENT_ROOT'] . '/uploads/' . $_POST["session_id"];
			if (!file_exists($location) && !is_dir($location)) {
				if(!mkdir($location, 0777, true)){
					$server_response['msg'] = "The directory $location could not be created ('mkdir' returned FALSE).";
					goto end;
				}
			}
			// clean data (and parameter, it's ok) files from possible previous run
			//error_log("cleanprev: $cleanprev file: $name");
			if($cleanprev){
				$txts = glob($location . "/*.txt");
				foreach($txts as $txt){
					if(is_file($txt)){
						unlink($txt);
					}
				}
			}
			if (move_uploaded_file($tmp_name, $location . '/' . $name)){
				// Keep calling "get_labels" till you get some labels (validity criteria for labelled experiments: 1: # of label names > 0. 2: if # of label definitions > 0 then label definitions cannot contain white-space characters)
				for($i=0; $i<count($get_labels_aguments_sets); $i++){
					$argset = $get_labels_aguments_sets[$i];
					$tmp = get_labels($location . '/' . $name,$argset[0],$argset[1],$argset[2]);
					if(count($tmp[0]) > 0 || $get_labels_aguments_sets_labelfree_flag[$i]){
						if(count($tmp[1]) > 0 ){
							//error_log("# of label defs (set $i): " . count($tmp[1]));
							$okdefs = 0;
							foreach($tmp[1] as $lbldef){
								if(preg_match('/[\s,;\:]/',$lbldef) == 0){
									$okdefs++;
								}else{
									array_push($server_response['skipped_labels'], $lbldef); 
								}
							}
							if($okdefs == count($tmp[1])){
								break;
							}
						}else{
							break;
						}
					}
				}
				$server_response['peptide_labels_names'] =  $tmp[0];
				$server_response['peptide_labels'] = $tmp[1];
				$server_response['success'] = true;
			}else{
				$server_response['msg'] = " The file $name could not be moved ('move_uploaded_file' returned FALSE).";
			}
		} else {
			$server_response['msg'] = "The variable 'name' was empty (empty() returned TRUE).";
		}
	}else{
		$server_response['msg'] = "The variable $name was not set ('isset' returned FALSE).";
	}

end:
	error_log("upload_files.php [" . $_POST["session_id"] . "]> Success: " . ($server_response['success'] ? 'Yes' : 'No') . " | Message: " . $server_response['msg']);
	//Send info back to the client
	header('Content-type: application/json');
	echo json_encode($server_response);
?>