 <?php
	require 'get_labels.php';
	// MaxQuant
	//$tmp = get_labels($argv[1],'/^Ratio ([^\s]+)\/([^\s]+)/',null,null);
	// PD
	//$tmp = get_labels($argv[1],'/^([^\s]+)\/([^\s]+)$/','/Modifications/','/\((?:Label:)?(.+?)\)(;|$)/');
	// PD label-free
	$tmp = get_labels($argv[1],null,'/Spectrum File/',null);
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
 ?>