<?php

require 'get_labels.php';
require 'get_rawfiles_names.php';
//"get_labels" function arguments for different file formats
$_get_labels_aguments_sets = array(
    'PD' => array(
        'L' => array(
            array('/^([^\s]+)\/([^\s]+)$/i', '/Modifications/i', '/\((?:[^:]+?:)?(.+?)\)(;|$)/'), //Proteome Discoverer
        ),
        'LF' => array(
            array(null, '/Spectrum File/i', null), // PD label-free
        )
    ),
    'MQ' => array(
        'L' => array(
            array('/^Ratio ([^\s]+)\/([^\s]+)/i', null, null), //MaxQuant
            array('/^Reporter intensity ([0-9]+)/i', null, null), //MaxQuant, MS/MS-multiplexing (reporter ions), e.g. iTRAQ
        ),
        'LF' => array(
            array(null, '/Raw file/i', null) //MaxQuant label-free
        )
    )
);

$server_side_file = ($_POST["server_side"] === "true");

if ($server_side_file) {
   $name = $_POST['thefile'];
   $tmp_name = $_POST['thefile'];
} else {
   $name = $_FILES['thefile']['name'];
   $tmp_name = $_FILES['thefile']['tmp_name'];

}
$server_response = [];
//Overall success, will be checked by client.
$server_response['success'] = false;
$server_response['msg'] = '';
$server_response['peptide_labels'] = [];
$server_response['peptide_labels_names'] = [];
$server_response['skipped_labels'] = [];
$server_response['raw_filesnames'] = [];
$server_response['file_type'] = '';
$document_root = dirname(__DIR__);

if (isset($name)) {
   if (!empty($name)) {
	   
      $location = $document_root . '/uploads/' . $_POST["session_id"];
      if (!file_exists($location) && !is_dir($location)) {
         if (!mkdir($location, 0777, true)) {
            $server_response['msg'] = "The directory $location could not be created ('mkdir' returned FALSE).";
            goto end;
         }
      }
      $file_copied_successfully = false;
      if ($server_side_file) {
         $file_copied_successfully = copy($document_root . "/test data/" . $tmp_name, $location . '/' . $name);
      } else {
		  if (!file_exists($location . "/" . $name))
		  {
			$file_copied_successfully = move_uploaded_file($tmp_name, $location . "/" . $name);
		  }
		  else
		  {
			$server_response['success'] = false;
			$server_response['msg'] = "A file with the same name ($name) has already been uploaded, try again later";
			goto end;
		  }
      }


      $handle = null;
      if ($file_copied_successfully && ($handle = fopen($location . '/' . $name, "r"))) {
         $first_line = fgetcsv($handle, 0, "\t");
         fclose($handle);
         $tmp = preg_grep("/Spectrum File/i", $first_line);
         if (count($tmp) > 0) {
            $dtype = 'PD';
         } else {
            $tmp = preg_grep("/Raw file/i", $first_line);
			if (count($tmp) > 0){
				$dtype = 'MQ';
			}
			else{
				$tmp = preg_grep("/Peptide IDs/i", $first_line);
				if (count($tmp) > 0){
					$dtype = 'MQP';
				}
				else{
					$dtype = 'unknown';
				}
			}
         }
         if ($dtype == 'MQ' || $dtype == 'PD') {
			 $server_response['file_type'] = $dtype;
            $get_labels_aguments_sets = array_merge(array_values($_get_labels_aguments_sets[$dtype]['L']), array_values($_get_labels_aguments_sets[$dtype]['LF']));
            $get_labels_aguments_sets_labelfree_flag = array_merge(array_values(array_fill(0, count($_get_labels_aguments_sets[$dtype]['L']), false)), array_values(array_fill(0, count($_get_labels_aguments_sets[$dtype]['LF']), true)));
            // Keep calling "get_labels" till you get some labels (validity criteria for labelled experiments: 1: # of label names > 0. 2: if # of label definitions > 0 then label definitions cannot contain white-space characters)
            for ($i = 0; $i < count($get_labels_aguments_sets); $i++) {
               $argset = $get_labels_aguments_sets[$i];
               $tmp = get_labels($location . '/' . $name, $argset[0], $argset[1], $argset[2]);
               if (count($tmp[0]) > 0 || $get_labels_aguments_sets_labelfree_flag[$i]) {
                  if (count($tmp[1]) > 0) {
                     //error_log("# of label defs (set $i): " . count($tmp[1]));
                     $okdefs = 0;
                     foreach ($tmp[1] as $lbldef) {
                        if (preg_match('/[\s,;\:]/i', $lbldef) == 0) {
                           $okdefs++;
                        } else {
                           array_push($server_response['skipped_labels'], $lbldef);
                        }
                     }
                     if ($okdefs == count($tmp[1])) {
                        break;
                     }
                  } else {
                     break;
                  }
               }
            }
            $server_response['peptide_labels_names'] = $tmp[0];
            $server_response['peptide_labels'] = $tmp[1];
            $server_response['raw_filesnames'] = get_rawfiles_names($location . '/' . $name, '/file/i');
            if (count($server_response['raw_filesnames']) == 0) {
               error_log("Could not retrieve replicate information (raw files names) from data file " . $name);
            }
            rename($location . '/' . $name, $location . '/msdiffexp_peptide.txt');
         } elseif ($dtype == 'MQP') {
			$server_response['file_type'] = $dtype;
            rename($location . '/' . $name, $location . '/msdiffexp_protein.txt');
         }
		 else{
			$server_response['file_type'] = "unknown";
			 unlink($location . '/' . $name);
			 $server_response['success'] = true;
			 $server_response['msg'] = "The file $name is not valid";
			 goto end;
		 }
         $server_response['success'] = true;
      } else {
         $server_response['msg'] = "The file $name could not be moved ('move_uploaded_file' returned FALSE).";
      }
   } else {
      $server_response['msg'] = "The variable 'name' was empty (empty() returned TRUE).";
   }
} else {
   $server_response['msg'] = "The variable $name was not set ('isset' returned FALSE).";
}

end:
error_log("upload_files.php [" . $_POST["session_id"] . " " . $name . " ]> Success: " . ($server_response['success'] ? 'Yes' : 'No') . " | Message: " . $server_response['msg']);
//Send info back to the client
header('Content-type: application/json');
echo json_encode($server_response);
?>