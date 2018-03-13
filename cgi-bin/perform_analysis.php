<?php
#Warning!!! the results_url var is hardcoded!! if you change the position of ProteoSign change str_replace($document_root, '/ProteoSign', $upload_dir . "/proteosign.zip") to str_replace($document_root, '/[new folder]', $upload_dir . "/proteosign.zip")
$server_response = [];
$server_response['success'] = false;
$server_response['msg'] = "";
$server_response['dispatcher_success'] = false;
$server_response['dispatcher_dump'] = [];
$server_response['R_success'] = false;
$server_response['R_dump'] = "";
$server_response['dump'] = "";
$server_response['ret_session'] = $_POST["session_id"];
$server_response['UserInfo'] = "";

$session_folder = $_POST["session_id"];
$document_root = dirname(__DIR__);
$upload_dir = $document_root . "/uploads/" . $session_folder;
// clean files from possible previous run
$pngs = glob($upload_dir . "/*.png");
foreach ($pngs as $png) {
   if (is_file($png)) {
      unlink($png);
   }
}
if (is_file($upload_dir . "/msdiffexp.zip")) {
   unlink($upload_dir . "/msdiffexp.zip");
}
// Run analysis
$cgibin_dir = getcwd();
chdir($upload_dir);
if (!file_exists("msdiffexp_wd"))
{
mkdir('msdiffexp_wd');
}
copy($cgibin_dir . '/MSdiffexp.R', $upload_dir . '/msdiffexp_wd/MSdiffexp.R');
copy($cgibin_dir . '/README.txt', $upload_dir . '/msdiffexp_wd/README.txt');
copy($cgibin_dir . '/Plot_Generator.R', $upload_dir . '/msdiffexp_wd/Plot_Generator.R');

rename('MSdiffexp_definitions.R', 'msdiffexp_wd/MSdiffexp_definitions.R');
rename('exp_struct.txt', 'msdiffexp_wd/exp_struct.txt');
rename('LFQ_conditions.txt', 'msdiffexp_wd/LFQ_conditions.txt');
if (file_exists("RMrawfiles.txt")) rename('RMrawfiles.txt', 'msdiffexp_wd/RMrawfiles.txt');
if (file_exists("RMtags.txt")) rename('RMtags.txt', 'msdiffexp_wd/RMtags.txt');
if ($_POST["AllowMergeLabels"] == "T") rename('Rename_array.txt', 'msdiffexp_wd/Rename_array.txt');
if ($_POST["AllowLS"] == "T") rename('LS_array.txt', 'msdiffexp_wd/LS_array.txt');
//in case the user changes session the files of the old session are copied to the new one, check if the procedure was done correctly before proceeding, if not, wait for the procedure to complete
$procprogram = $_POST["proc_program"];
if ($procprogram == "MQ")
{
	for ($i = 0; $i < 16; $i++)
	{
		if(!file_exists('msdiffexp_protein.txt') || !file_exists('msdiffexp_peptide.txt'))
		{
			sleep(2);
		}
		else
		{
			break;
		}
	}
	if(!file_exists('msdiffexp_protein.txt') || !file_exists('msdiffexp_peptide.txt'))
	{
		$server_response['msg'] .= "Uploaded files not found! Please try again ";
		goto end;
	}
}
else
{
	for ($i = 0; $i < 16; $i++)
	{
		if(!file_exists('msdiffexp_peptide.txt'))
		{
			sleep(2);
		}
		else
		{
			break;
		}
	}
	if(!file_exists('msdiffexp_peptide.txt'))
	{
		$server_response['msg'] .= "Uploaded files not found! Please try again ";
		goto end;
	}
}
sleep(2);
rename('msdiffexp_peptide.txt', 'msdiffexp_wd/msdiffexp_peptide.txt');
if(file_exists('msdiffexp_protein.txt'))
{
	rename('msdiffexp_protein.txt', 'msdiffexp_wd/msdiffexp_protein.txt');
}
chdir('msdiffexp_wd');

//TODO rewrite the following command to UNIX compatible format
exec('RScript.exe MSdiffexp.R > msdiffexp_log.txt');
// Determine success of R run by search for 'error' occurrences in msdiffexp_log.txt
$R_logfile = file_get_contents("msdiffexp_log.txt");
if ($R_logfile) {
   $r_logfile_error_lines = [];
   if (preg_match_all("/Error (.*)/s", $R_logfile, $r_logfile_error_lines) > 0) {
      $server_response['R_dump'] = implode(" -*- Error -*-\n", array_values($r_logfile_error_lines[1]));
      $server_response['msg'] = "The R script returned error(s).";
      $server_response['dump'] = $server_response['R_dump'];
   } else {
      $server_response['R_success'] = true;
   }
   if (preg_match_all("/(Warn User:.*|Error User:.*|Info User:.*)/", $R_logfile, $r_logfile_error_lines) > 0) {
	   $server_response['UserInfo'] .= implode(array_values($r_logfile_error_lines[1]), "\t\t");
   }
}
if (!file_exists("msdiffexp_out"))
{
	mkdir("msdiffexp_out");
}
rename('msdiffexp_log.txt', 'msdiffexp_out/log.txt');
rename('README.txt', 'msdiffexp_out/README.txt');
rename('Plot_Generator.R', 'msdiffexp_out/Plot_Generator.R');
chdir('..');
foreach (glob("*_parameters_from_session_*.txt") as $filename) {
	rename($filename, 'msdiffexp_wd/msdiffexp_out/' . $filename);
}
chdir($upload_dir);
if ($server_response['R_success']) {
	//WIN TODO
	//Windows compatibility issue may occur:
   exec("copy msdiffexp_wd\\msdiffexp_out\\*.pdf .; mogrify -format png -density 150 -quality 100 -fill white -opaque none *.pdf; rm *.pdf");
   //Windows can not zip a file using native commands, the following command uses 7zip command line version (http://7-zip.org/)
   exec("7za a -tzip msdiffexp.zip msdiffexp_wd\\msdiffexp_out\\*.*");
}


// Determine success of analysis
$server_response['success'] = $server_response['R_success'];
if ($server_response['success']) {
   // $server_response['results_url'] = $upload_dir . "/msdiffexp.zip";
   // NOT LOCALHOST DEPLOYMENT VERSION
   $server_response['results_url'] = str_replace($document_root, '', $upload_dir . "/msdiffexp.zip");
   //The next line is for windows compatibility: WIN TODO
   $server_response['results_url'] = str_replace('\\', '/', $server_response['results_url']); 
   //Removing the first slash:
   $server_response['results_url'] = substr($server_response['results_url'], 1);
   //
   $server_response['results_preview'] = [];
   $pngs = glob("msdiffexp_wd/*.png");
   // LOCALHOST DEPLOYMENT VERSION ONLY
   // $server_response['results_preview'] = $pngs;
   //
		// NOT LOCALHOST DEPLOYMENT VERSION
   foreach ($pngs as $png_i) {
      $server_response['results_preview'][] = substr(str_replace($document_root, '', $upload_dir . "/" . $png_i), 1);
   }
   //
}
end:
error_log("client: [" . $_SERVER['REMOTE_ADDR'] . "]" . " perform_analysis.php [" . $_POST["session_id"] . "]> Success: " . ($server_response['success'] ? 'Yes' : 'No') . " | Message: " . $server_response['msg']);
if (!$server_response['success']) {
   error_log("perform_analysis.php [" . $_POST["session_id"] . "]> Relevant dump: " . $server_response['dump']);
} else {
	
   // exec('del -Rf msdiffexp_wd');
}
//Send info back to the client
//error_log(json_encode($server_response));
header('Content-type: application/json');
echo json_encode($server_response);
?>
        
