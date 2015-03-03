<?php

require dirname(__DIR__) . '/../cgi-bin/get_labels.php';
require dirname(__DIR__) . '/../cgi-bin/get_rawfiles_names.php';
//"get_labels" function arguments for different file formats
$_get_labels_aguments_sets = array(
    'PD' => array(
        'L' => array(
            array('/^([^\s]+)\/([^\s]+)$/', '/Modifications/', '/\((?:[^:]+?:)?(.+?)\)(;|$)/'), //Proteome Discoverer
        ),
        'LF' => array(
            array(null, '/Spectrum File/', null), // PD label-free
        )
    ),
    'MQ' => array(
        'L' => array(
            array('/^Ratio ([^\s]+)\/([^\s]+)/', null, null), //MaxQuant
            array('/^Reporter intensity ([0-9]+)/', null, null), //MaxQuant, MS/MS-multiplexing (reporter ions), e.g. iTRAQ
        ),
        'LF' => array(
            array(null, '/Raw file/', null) //MaxQuant label-free
        )
    )
);

$name = $argv[1];
$data_file = $name;
$server_response = [];
//Overall success, will be checked by client.
$server_response['success'] = false;
$server_response['msg'] = "";
$server_response['peptide_labels'] = [];
$server_response['peptide_labels_names'] = [];
$server_response['skipped_labels'] = [];
$server_response['raw_filesnames'] = [];
$document_root = $_SERVER['DOCUMENT_ROOT'] . "/ProteoSign";
if (isset($name)) {
   if (!empty($name)) {
      $handle = null;
      $file_copied_successfully = true;
      if ($file_copied_successfully && ($handle = fopen($data_file, "r"))) {
         $dtype = (array_search('Spectrum File' , fgetcsv($handle, 0, "\t"), true) === true ? 'PD' : 'MQ');
         fclose($handle);
         $get_labels_aguments_sets = array_merge(array_values($_get_labels_aguments_sets[$dtype]['L']),array_values($_get_labels_aguments_sets[$dtype]['LF']));
         $get_labels_aguments_sets_labelfree_flag = array_merge(array_values(array_fill(0,count($_get_labels_aguments_sets[$dtype]['L']),false)),  array_values(array_fill(0,count($_get_labels_aguments_sets[$dtype]['LF']),true)));
         // Keep calling "get_labels" till you get some labels (validity criteria for labelled experiments: 1: # of label names > 0. 2: if # of label definitions > 0 then label definitions cannot contain white-space characters)
         for ($i = 0; $i < count($get_labels_aguments_sets); $i++) {
            $argset = $get_labels_aguments_sets[$i];
            $tmp = get_labels($name, $argset[0], $argset[1], $argset[2]);
            if (count($tmp[0]) > 0 || $get_labels_aguments_sets_labelfree_flag[$i]) {
               if (count($tmp[1]) > 0) {
                  //error_log("# of label defs (set $i): " . count($tmp[1]));
                  $okdefs = 0;
                  foreach ($tmp[1] as $lbldef) {
                     if (preg_match('/[\s,;\:]/', $lbldef) == 0) {
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
         $server_response['success'] = true;
      } else {
         $server_response['msg'] = "The file $name could not be moved/read (either 'move_uploaded_file' or 'fopen' returned FALSE).";
      }
   } else {
      $server_response['msg'] = "The variable 'name' was empty (empty() returned TRUE).";
   }
} else {
   $server_response['msg'] = "The variable $name was not set ('isset' returned FALSE).";
}

end:
echo print_r($server_response, true);
?>