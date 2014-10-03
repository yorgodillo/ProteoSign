<?php
	function get_rawfiles_names($data_file, $labeldefcol_re){
		$rawfiles_names = [];
		$ret = [];
		$labeldefcol = -1;
		if (($handle = fopen($data_file, "r")) !== FALSE) {
			if (($data = fgetcsv($handle, 0, "\t")) !== FALSE) {
				$ncols = count($data);
				// find the column which contains the raw data file names
				for ($c=0; $c < $ncols; $c++) {
					$matches = [];
					if(preg_match($labeldefcol_re, $data[$c])){
						$labeldefcol = $c;
						break;
					}
				}
				if($labeldefcol > -1){
					while (($data = fgetcsv($handle, 0, "\t")) !== FALSE) {
						array_push($rawfiles_names, $data[$labeldefcol]);
					}
					// extract unique file names, do not use the PHP function 'array_unique' (it is slower and preserves the array keys, doesn't make new keys)!
					$ret = array_keys(array_flip($rawfiles_names));
				}
			}
			fclose($handle);
		}
		error_log(print_r($ret, true));
		return $ret;
	}
 ?>