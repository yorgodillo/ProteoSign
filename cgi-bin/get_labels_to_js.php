<?php
	$document_root = dirname(__DIR__);
	$location = $document_root . '/uploads/' . $_POST["session_id"];
	// //DEBUG:
	// $location = "C:/xampp/htdocs/ProteoSign/uploads/1477755182769";
	  $name = 'msdiffexp_peptide.txt';
	  $data_file = $location . '/' . $name;
	  $server_response['labels'] = [];
	  $server_response['isIsobaricLabel'] = false;
	  $server_response['success'] = false;
	  $server_response['msg'] = "";
	  if (!file_exists($location) && !is_dir($location)) {
            $server_response['msg'] = "The directory $location could not be found! at get_labels_to_js.";
            goto end;
      }
	  // $temp = "";
	if (($handle = fopen($data_file, "r")) !== FALSE) {
		$titles = explode("\t", fgets($handle));
		$pattern = '/^Intensity .*/i';
		$pattern2 = '/^Reporter intensity/i';
		$pattern3 = '/^\d*\/\d*$/';
		$counter = 0;
		foreach ($titles as &$value) {
			// $temp .= $value;
			$value = preg_replace("~^\"~", "", $value);
			$value = preg_replace("~\"$~", "", $value);
			preg_match($pattern, $value, $matches);
			if (count($matches) > 0)
			{
				$server_response['labels'][$counter] = substr($matches[0], 10);
				$counter++;
			}
			preg_match($pattern2, $value, $matches);
			if (count($matches) > 0)
			{
				$server_response['isIsobaricLabel'] = true;
			}
			preg_match($pattern3, $value, $matches);
			if (count($matches) > 0)
			{
				$server_response['isIsobaricLabel'] = true;
			}
		}
		fclose($handle);
		$server_response['success'] = true;
	}
	end:
	// $server_response['msg'] = $temp;
	error_log("get_labels_to_js.php [" . $_POST["session_id"] . " " . $name . " ]> Success: " . ($server_response['success'] ? 'Yes' : 'No') . " | Message: " . $server_response['msg']);
	//Send info back to the client
	header('Content-type: application/json');
	echo json_encode($server_response);
?>