 <?php
	require dirname(__DIR__) . '/../cgi-bin/get_labels.php';
	require dirname(__DIR__) . '/../cgi-bin/get_rawfiles_names.php';
	
	// MaxQuant
	$tmp = get_labels($argv[1],'/^Ratio ([^\s]+)\/([^\s]+)/',null,null);
   //$tmp = get_labels($argv[1],'/^Reporter intensity ([0-9]+)/',null,null);
	// PD
	//$tmp = get_labels($argv[1],'/^([^\s]+)\/([^\s]+)$/','/Modifications/','/\((?:[^:]+?:)?(.+?)\)(;|$)/');
	// PD label-free
	//$tmp = get_labels($argv[1],null,'/Spectrum File/',null);
	// MaxQuant label-free
	//$tmp = get_labels($argv[1],null,'/Raw file/',null);
	$labels_names = $tmp[0];
	$labels = $tmp[1];

	foreach($labels_names as $name){
		echo "lbl name: $name\n";
	}	
	
	foreach($labels as $label){
		echo "lbl: $label\n";
	}
	
	//error_log(print_r(get_rawfiles_names($argv[1],'/file/i'), true));
	
 ?>