 <?php
	 /*
	 This php file makes a copy of asession to a new one so that the same files can be run 
	 */
	 $document_root = dirname(__DIR__);
	 $old_location = $document_root . '/uploads/' . $_POST["old_session_id"];
	 $new_location = $document_root . '/uploads/' . $_POST["session_id"];
	 $server_response['success'] = true;
	 $server_response['msg'] = '';
	 if (!file_exists($new_location))
	 {
		 mkdir($new_location, 0777, true);
	 }
	 if (file_exists($old_location . '/msdiffexp_wd/msdiffexp_peptide.txt'))
	 {
		 copy($old_location . '/msdiffexp_wd/msdiffexp_peptide.txt', $new_location . '/msdiffexp_peptide.txt');
	 }
	 else
	 {
		 $server_response['success'] = false;
		 $server_response['msg'] .= 'msdiffexp_peptide not found';
	 }
	 if (file_exists($old_location . '/msdiffexp_wd/msdiffexp_protein.txt'))
	 {
		 copy($old_location . '/msdiffexp_wd/msdiffexp_protein.txt', $new_location . '/msdiffexp_protein.txt');
	 }
	 else{
		 $server_response['msg'] .= 'msdiffexp_protein not found, probably working on a PD dataset';
	 }
	error_log("change_session.php [" . $_POST["session_id"] . " ]> Success: " . ($server_response['success'] ? 'Yes' : 'No') . " | Message: " . $server_response['msg']);
	//Send info back to the client
	header('Content-type: application/json');
	echo json_encode($server_response);
 ?>