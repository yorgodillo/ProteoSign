<?php
	function get_labels($data_file,$labelmatch_re,$labeldefcol_re,$labeldef_re){
		$labels = [];
		$labels_defs = [];
		$labeldefcol = -1;
		$row = 0;
		if (($handle = fopen($data_file, "r")) !== FALSE) {
			if (($data = fgetcsv($handle, 0, "\t")) !== FALSE) {
				$row++;
				$ncols = count($data);
				// if we have labelled data (not label-free)
				if(isset($labelmatch_re)){
					for ($c=0; $c < $ncols; $c++) {
						$matches = [];
						if(preg_match_all($labelmatch_re,$data[$c],$matches) > 0){
							foreach($matches[1] as $match){
								#echo "$match ($data[$c])\n";
								if(!array_key_exists($match, $labels)){
									$labels[$match] = 1;
								}
							}						
							foreach($matches[2] as $match){
								#echo "$match ($data[$c])\n";
								if(!array_key_exists($match, $labels)){
									$labels[$match] = 1;
								}
							}						
						}
						
						if(isset($labeldefcol_re) && $labeldefcol < 0 && preg_match($labeldefcol_re, $data[$c])){
							$labeldefcol = $c;
						}
					}
				}else{
					for ($c=0; $c < $ncols; $c++) {
						$matches = [];
						if(preg_match($labeldefcol_re, $data[$c])){
							$labeldefcol = $c;
							break;
						}
					}
				}
				// if we have labelled data (not label-free)
				if(isset($labelmatch_re)){
					while ($labeldefcol > -1 && ($data = fgetcsv($handle, 0, "\t")) !== FALSE) {
						$matches = [];
						$row++;
						#echo "SEARCHING line $row: $data[$labeldefcol]\n";
						if(preg_match_all($labeldef_re,$data[$labeldefcol],$matches) > 0){
							foreach($matches[1] as $match){
								#echo "line $row: $match\n";
								# Label definitions must be more than one letter long (Ad hoc)
								if(strlen($match) > 1 && !array_key_exists($match, $labels_defs)){
									#echo "MATCHED: '$match' at line $row\n";
									$labels_defs[$match] = 1;
								}
							}
						}
					}
				}else{
					while ($labeldefcol > -1 && ($data = fgetcsv($handle, 0, "\t")) !== FALSE) {
						$matches = [];
						$row++;
						#echo "SEARCHING line $row: $data[$labeldefcol]\n";
						$rawdata_filename_wext = $data[$labeldefcol];
						if(preg_match_all('/(.+?)(\.[^.]*$|$)/',$rawdata_filename_wext,$matches) > 0){
							foreach($matches[1] as $match){
								if(strlen($match) > 1 && !array_key_exists($match, $labels_defs)){
									$labels_defs[$match] = 1;
									break;
								}
							}
						}
					}
				}
			}
			fclose($handle);
		}
		return [array_keys($labels),array_keys($labels_defs)];
	}
 ?>