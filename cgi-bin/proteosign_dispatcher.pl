use warnings;
use strict;

  use Cwd;
  use File::Basename;
  use File::Copy;  
  use File::stat;
  use Archive::Zip qw( :ERROR_CODES :CONSTANTS );
  use File::Path qw(make_path remove_tree);
  use DateTime::Format::Strptime qw( );
  use File::Which;
  use File::Which qw(which where);
  STDOUT->autoflush(1); # turn-off STDOUT buffering

  #print "[LOG] running as " . getpwuid( $< ) . "\n";

  # Windows path
  if($^O =~ /MSWin/){
	my ($base,$IM_path,$R_path,$type);
	($base,$IM_path,$type) = fileparse(which('mogrify.exe'));
	($base,$R_path,$type) = fileparse(which('R.exe'));
	local $ENV{PATH} = "$ENV{PATH};$R_path";
	local $ENV{PATH} = "$ENV{PATH};$IM_path";
	}
  
  my $datetime_f = DateTime::Format::Strptime->new( pattern => '%Y%m%d_%H%M%S' );  
  
  if($#ARGV < 0){
    print "USAGE: perl proteosign_dispatcher <Parameters FILE> [<PSMs FILE> [<Protein FILE>]]\n";
    exit;
  }
  
  # Assume all files in directory of param file, except the param file and the exp_struct files, are input data files
  if($#ARGV == 0){
	my ($tmp_fname, $tmp_absparampath, $tmp_dummy) = fileparse(File::Spec->rel2abs( $ARGV[0] ));
	opendir(DIR, $tmp_absparampath) or die "[LOG] proteosign_dispatcher: ERROR! Can't open directory.\n";
    while (my $datafile_i = readdir(DIR)) {
        # Use a regular expression to ignore file beginning with a period, is not $ARGV[0] and not a directory
        next if ($datafile_i =~ m/^\./) || ($datafile_i !~ m/.txt$/) || ($datafile_i =~ m/$tmp_fname/) || ($datafile_i =~ m/exp_struct.txt/) || (-d "$tmp_absparampath$datafile_i");
		push(@ARGV,"$tmp_absparampath$datafile_i");
    }
    closedir(DIR);	
  }

  &doit(join(',',splice(@ARGV))) ;
  
  exit;

sub doit {
  my ( $files  ) = @_ ;
  my @files = split(/,/,$files);
  my $i = 0;
  my $input_path = "";
  my $tmp;
  for($i=0; $i<@files; $i++){
	if (! -s $files[$i]) { die "[LOG] proteosign_dispatcher: ERROR! Can't find(or empty) file " .  $files[$i] . "\n" ;}
	($files[$i], $input_path, $tmp) = fileparse(File::Spec->rel2abs( $files[$i] ));
  }

  $input_path=~ s/\\/\//g;
  $input_path=~ s/\/$//g;
  
  my $launch_path = getcwd;
  chdir $input_path . "/";
    
  open TMP, '>', "msdiffexp_in_TMP" or die "[LOG] proteosign_dispatcher: " . $! . "\n";
  
  my @file_sizes = ();
  my @file_headers = ("[Parameter]","[Peptide]","[Protein]");
  #concatenate parameter/peptide/protein files
  $i = 0;
  foreach(@files){
    open(FILEN, "<" . $_) or die "[LOG] proteosign_dispatcher: " . $! . "\n";
    print TMP $file_headers[$i++] . "\n";
    print TMP <FILEN>;
    close(FILEN);
    push(@file_sizes, stat($_)->size);
  }
  close(TMP);
  
  my $file_size = stat("msdiffexp_in_TMP")->size;
  #make header of concatenated file (for server script interpretation)
  my $file_header = $file_size . "\n" . $file_sizes[1] . "\n"; # $file_sizes[1] size in bytes of the first (and maybe last) dataset in bytes
  
  #create file and add contents (header + peptide and protein (if given) files contents)
  my $data_fname = "msdiffexp";
  my $success = open(MSDIFFEXPIN, ">" . $data_fname);
  if(! $success){
    unlink "msdiffexp_in_TMP";
    die "[LOG] proteosign_dispatcher: " . $! . "\n";
  }
  print "[LOG] proteosign_dispatcher: Packing the input ... ";
  binmode(MSDIFFEXPIN);
  $success = open(TMP, "<" . "msdiffexp_in_TMP");
  if(! $success){
    close(MSDIFFEXPIN);
    unlink $data_fname;
    die "[LOG]  proteosign_dispatcher: " . $! . "\n";
  }
  binmode(TMP);
  print MSDIFFEXPIN $file_header;
  print MSDIFFEXPIN <TMP>;
  close(TMP);
  close(MSDIFFEXPIN);
  print "DONE\n";
  
  unlink "msdiffexp_in_TMP";

  # WARNING! Whenever touching the last 2 keys of the hash below, be sure to also update the lines 194 and 272 accordingly.
  my %params_matchmap = ('^Proteome Discoverer data','PDdata',
'^Experiment ID','outputFigsPrefix',
'^Time point','time.point',
'^Protein \(as opposed to peptide\)','ProteinQuantitation',
'^Enable quantitation filtering','filterL',
'^If the above two are enabled','filterL_lvl',
'^If filtering is enabled','filterL_lbl',
'^Label\-free experiment','LabelFree',
'^Label\s','addLabel( ,c(	))',
'^Modification','addMod( ,c(	))');

  my %parsed_params = ('PDdata','',
'outputFigsPrefix','',
'time.point','',
'ProteinQuantitation','',
'filterL','',
'filterL_lvl','',
'filterL_lbl','',
'LabelFree','',
'label','',	# Gia ta matia tou kosmou (not used, exists for code consitency)
'modification','');	# Gia ta matia tou kosmou (not used, exists for code consitency)

  my %params_desc = ('PDdata','Proteome Discoverer quantitation (Yes/No)',
'outputFigsPrefix','Experiment ID',
'time.point','Time point',
'ProteinQuantitation','Quantitation level (Protein/Peptide)',
'filterL','Quantitation filtering (Yes/No)',
'filterL_lvl','Quantitation filtering level (Peptide/Protein)',
'filterL_lbl','Quantitation filtering label/modification',
'LabelFree','Label-free experiment (Yes/No)');

  my @found_params = ();

 
  my $working_dir_name = $data_fname . "_wd";
  
  mkdir $working_dir_name unless -d $working_dir_name;
  print "[LOG] proteosign_dispatcher: Creating the analysis parameter file ... ";
  open(RDEFFILE, ">$working_dir_name/MSdiffexp_definitions.R") or die "[ERROR] msdiffexp_dsipatcher: " . $! . "\n";
  if(! open(DATAFILE, "<" . $data_fname)){
    close(RDEFFILE);
    die "[ERROR] msdiffexp_dsipatcher: " . $! . "\n";
  }
  
  my $line = <DATAFILE>;
  $line = <DATAFILE>;
  my $eof_header = 0;
  # reach end of header
  while(! $eof_header && defined($line)){
    chomp($line);
    if($line =~ m/^\[Param/){
      $eof_header = 1;
      next;
    }
    $line = <DATAFILE>;
  }
  if(! $eof_header){
    close(DATAFILE);
    close(RDEFFILE);
    die "[ERROR!] msdiffexp_dsipatcher: Experimental parameters were not specified by the user.\n";
  }
  
  # parse parameters
  $line = <DATAFILE>;
  my $eof_params = 0;
  my @defined_labels = ();
  my @defined_modifications = ();
  $parsed_params{'label'} = \@defined_labels;
  $parsed_params{'modification'} = \@defined_modifications;
  
  while(! $eof_params && defined($line)){
    chomp($line);
    if($line =~ m/^\[Pept/){
      $eof_params = 1;
      next;
    }
    if($line !~ m/^#/){
    #print "*". $line . "\n";
    #go through all yet to be found params and see if the current line matches
    foreach my $curr_param (keys %params_matchmap)
    {
      if($line =~ m/$curr_param/i){
        my @param_val = split(/\t/, $line);
        #print "MATCHED TO $curr_param ($param_val[1])\n";
        if($curr_param !~ m/^\^(Label\\s|Modification)/i){
			  if(scalar(@param_val) > 1){  
				 if($param_val[1] !~ s/^\s*Yes\s*$/T/){
				   if($param_val[1] !~ s/^\s*No\s*$/F/){
				     if($param_val[1] !~ m/^\s*[0-9]+\s*$/){
				       if($param_val[1] !~ m/[^,]+(,[^,]+)+/){
				       	 # double-quote value
						    $param_val[1] = '"' . $param_val[1] . '"';
						 }else{
						 	 # double-quote comma-separated values
						 	 $param_val[1] = '"' . join('","',split(/,/, $param_val[1])) . '"';
						 }
						 $param_val[1] =~ s/\s/_/;
				     }
				   }
				 }
			  }else{
			    push(@param_val, '""');
			  }
          #print "  " . $params_matchmap{$curr_param} . "=" . $param_val[1] . "\n";
          $parsed_params{$params_matchmap{$curr_param}} = $param_val[1];
          push(@found_params, $curr_param);
        }else{
          my $lblmod_entry = $params_matchmap{$curr_param};
          $param_val[1] = '"' . $param_val[1] . '"';
          if(defined($param_val[2])){
		       if($param_val[2] !~ m/[^,]+(,[^,]+)+/){
		       	 # double-quote value
				    $param_val[2] = '"' . $param_val[2] . '"';
				 }else{
				 	 # double-quote comma-separated values
				 	 $param_val[2] = '"' . join('","',split(/,/, $param_val[2])) . '"';
				 }
            #$param_val[2] =~ s/^([^,]*)/\"$1\"/;
            #$param_val[2] =~ s/\",(.*)$/\",\"$1\"/;
            $lblmod_entry =~ s/\t/$param_val[2]/;
          }else{
            $lblmod_entry =~ s/\t/\"\"/;
          }
          $lblmod_entry =~ s/ /$param_val[1]/;
          
          #print $params_matchmap{$curr_param} . "=" . $lblmod_entry . "\n";
          if($curr_param =~ m/label/i){
            push(@defined_labels, $lblmod_entry);
          }else{
            push(@defined_modifications, $lblmod_entry);
          }
        }
        last;
      }
    }
    }
    
    $line = <DATAFILE>;
  }
  
  if(@defined_labels < 2){
    close(DATAFILE);
    close(RDEFFILE);
    die "[ERROR!] msdiffexp_dsipatcher: At least two labels have to be specified by the user for the capability of relative quantitation.\n";
  }
  
  
  if(($#found_params+1) != ((keys %params_matchmap)-2)){ #-2 because we don't count the last two (label and modification) definitions
    close(DATAFILE);
    close(RDEFFILE);
    my @not_found_params = ();
    foreach my $curr_param (keys %params_matchmap){
      if(! grep {$_ eq $curr_param} @found_params){
        push(@not_found_params, $curr_param);
      }
    }
    my $n_nfp = ($#not_found_params+1);
    die "[ERROR!] msdiffexp_dsipatcher: " . $n_nfp . " experimental parameter" . ($n_nfp > 1 ? 's' : '') . " (" . join(", ", map { "$params_desc{$_}" } @not_found_params) . ")" . ($n_nfp > 1 ? 'were' : 'was') . " not specified by the user.\n";
  }
  
  # write MSdiffexp_definitions.R
  print RDEFFILE 'ratios.hist.colour<-"cyan"
reps.scatter.lmline.colour<-"red"
nRequiredLeastBioreps<-2
GUI<-F
paramssetfromGUI<-F
keepEvidenceIDs<-F
exportFormat<-"pdf"
rep_order<-NA
mqValidation<-F
working_directory<-"' . $input_path . '/' . $working_dir_name . '"
pgroups_fname<-"' . $data_fname . '_protein.txt"
evidence_fname<-"' . $data_fname . '_peptide.txt"
experimental_structure_file<-"exp_struct.txt"
';
 
 #write params
 foreach my $curr_param (keys %params_matchmap){
   if($curr_param !~ m/^\^(Label\\s|Modification)/i){
      my $line = $params_matchmap{$curr_param} . "<-" . $parsed_params{$params_matchmap{$curr_param}} . "\n";
      $line=~ s/\x0D//g;
     print RDEFFILE $line;
   }
 }
 #write labels
 foreach my $curr_lbl (@defined_labels){
   print RDEFFILE $curr_lbl . "\n";
 }
 #write modifications
 foreach my $curr_mod (@defined_modifications){
   print RDEFFILE $curr_mod . "\n";
 } 
 close(RDEFFILE);
 
 print "DONE\n";
 print "[LOG] proteosign_dispatcher: Creating the data file(s) ... ";
 #copy rest of contents to a peptide and/or protein file.
 $line = <DATAFILE>; # skip '[Peptide]' line
 open(PEPTFILE, ">$working_dir_name/" . $data_fname . "_peptide.txt") or die "[ERROR] msdiffexp_dsipatcher: " . $! . "\n";
 open(PROTFILE, ">$working_dir_name/" . $data_fname . "_protein.txt") or die "[ERROR] msdiffexp_dsipatcher: " . $! . "\n";
 my $eof_peptide = 0;
  while(! $eof_peptide && defined($line)){
    if($line =~ m/^\[Prot/){
      $eof_peptide = 1;
      next;
    }
    print PEPTFILE $line;
    $line = <DATAFILE>;
  }
  close(PEPTFILE);
  
  $line = <DATAFILE>; # skip '[Protein]' line
  if($eof_peptide){
    my $eof_protein = 0;
    while(defined($line)){
      print PROTFILE $line;
      $line = <DATAFILE>;
    }
  }
 close(PROTFILE);
 close(DATAFILE);
 print "DONE\n";
 #do the analysis and send the results
 my $os_name = $^O;

	copy($data_fname, $working_dir_name . "/" . $data_fname);
	unlink($data_fname);
	copy("exp_struct.txt", $working_dir_name . "/exp_struct.txt");
	copy($launch_path . "/" . "MSdiffexp.R", $working_dir_name . "/MSdiffexp.R");
	 
	 my $beforechdir = cwd();
	 #execute R-script and wait for it to finish
	 chdir($working_dir_name);
	 
	 print "[LOG] proteosign_dispatcher: Performing the analysis ... ";
	 
	 system("R CMD BATCH --slave MSdiffexp.R " . $data_fname . "_log.txt");	 
	 print "DONE\n";
	 #
	 move("curr_exp_design.txt", $working_dir_name . "/curr_exp_design.txt");
	 copy($data_fname . "_log.txt", $beforechdir);
	 my @pdfs = glob("msdiffexp_out/*.pdf");
	 foreach (@pdfs) {
		copy($_,$beforechdir);
	}
	 #delete source files
	 #unlink("MSdiffexp_definitions.R");
	 unlink("MSdiffexp.R");
	 unlink($data_fname);
	 # Commnet the following two lines when debugging
	 #unlink($data_fname . "_peptide.txt");
	 #unlink($data_fname . "_protein.txt");
	 chdir($beforechdir);
	 print "[LOG] proteosign_dispatcher: Generating the thumbnails ... ";
	 system("mogrify -format png -density 150 -quality 100 -fill white -opaque none *.pdf");
	 print "DONE\n";
	@pdfs = glob("*.pdf");
	foreach (@pdfs) {
		unlink($_);
	}
	 print "[LOG] proteosign_dispatcher: Packing the results ... ";
	 #compress the whole folder
	 my $zip = Archive::Zip->new();
	 my $dir_member = $zip->addTree($data_fname . "_wd", $data_fname );
	 my $zipret = $zip->writeToFileNamed($data_fname . ".zip");
	 unless ( $zipret == AZ_OK ) {
	     die "[ERROR!] msdiffexp_dsipatcher: Compression of " . $data_fname . "_wd failed (#" . $zipret . ").\n";
	 }
	 remove_tree($working_dir_name);
	print "DONE\n";
	 
  my $fullpathto_results = cwd() . "/" . $data_fname . ".zip";  
}

sub date_time_str{
  return $datetime_f->format_datetime(DateTime->now(time_zone => 'local'));
}
