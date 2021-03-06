var sessionid = new Date().getTime();
var clientname = '';
var softversion = '';
var cgi_bin_path = 'cgi-bin/';
var uploaded_files = 0;
var upload_was_successful = false;
var types_of_files_uploaded = [];
var nToUpload = 0;
var tmp_nToUpload = 0;
//procProgram refers to the program that is believed to have processed the raw data (MQ or PD) based on the uploaded files' format
var procProgram = "";
// from: http://www.shamasis.net/2009/09/fast-algorithm-to-find-unique-items-in-javascript-array/
Array.prototype.unique = function () {
   var o = {}, i, l = this.length, r = [];
   for (i = 0; i < l; i += 1)
      o[this[i]] = this[i];
   for (i in o)
      r.push(o[i]);
   return r;
}

// for counting the number of elements in an associative array
// from: http://stackoverflow.com/questions/5223/length-of-javascript-object-ie-associative-array
Object.size = function (obj) {
   var size = 0, key;
   for (key in obj) {
      if (obj.hasOwnProperty(key))
         size++;
   }
   return size;
}
// for filtering filetypes
function filterFunction(value) {
	if (procProgram == "MQ")
	{
		return (value == "MQ" || value == "MQP");
	}
	else if( procProgram == "PD")
	{
		return (value == "PD");
	}
}
//http://dzone.com/snippets/array-shuffle-javascript
function shuffle(o) {
   for (var j, x, i = o.length; i; j = Math.floor(Math.random() * i), x = o[--i], o[i] = o[j], o[j] = x)
      ;
   return o;
}
;
File.prototype.toString = function getFileName() {
   return this.name;
}

var rawfiles_tbl_allfiles_DT;

var onChooseFromTestDatasets = function(){
	$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#s1TestDatasets").width()/2)});
	$('body').append('<div id="mask"></div>');
	$("#s1TestDatasets").fadeIn(300);
	$('#mask').fadeIn(300);
}
	
var inputChCheck = function (e, repatt, maxCharacters) {
   var theEvent = e || window.event;
   var key = theEvent.keyCode || theEvent.which;
   if (key === 8) {
      return;
   }
   var re = new RegExp(repatt);
   var srcelem = e.target || e.srcElement;
   var txt = srcelem.value + String.fromCharCode(e.which);
   if (!re.test(txt) || txt.length > maxCharacters) {
      e.preventDefault();
   }
}

// from http://codereview.stackexchange.com/questions/7001/better-way-to-generate-all-combinations
var combinations = function (str) {
   var fn = function (active, rest, a) {
      if (!active && !rest)
         return;
      if (!rest) {
         a.push(active);
      } else {
         fn(active + rest[0], rest.slice(1), a);
         fn(active, rest.slice(1), a);
      }
      return a;
   }
   return fn("", str, []);
}

var combinations_arr = function (arr, delim) {
   var ret = [];
   var str = "0123456789";
   var combs = combinations(str.slice(0, arr.length));
   for (var i in combs) {
      var comb_i = combs[i];
      var ret_i = "";
      ret_i = arr[str.indexOf(comb_i[0])];
      for (var i = 1; i < comb_i.length; i++) {
         ret_i = ret_i + delim + arr[str.indexOf(comb_i[i])];
      }
      ret.push(ret_i);
   }
   return ret;
}

// From an array of items matched by "selector", choose the one with index "idx" and set some attribute(s)
var helper_setItemArrAttr = function (selector, idx, json) {
   $($(selector).get(idx)).attr(json);
}

// For a single item matched by "selector" set some attribute(s)
var helper_setItemAttr = function (selector, json) {
   var x = $(selector).attr(json);
}

// Adds/Removes ("_removeClass" false/true) class "className" in succession (each time its called) for a list of items.
// Assumes ONLY ONE item in the list "itms" SHOULD/SHOULD NOT ("_removeClass" false/true) have the class "className".
// toggleFun executes code with the current (0-based) index as a parameter.
var toggleNextClass = function (itms, className, _removeClass, toggleFun) {
   var nToggle = 0;
   var toggleClassFunction = (_removeClass ? ["removeClass", "addClass"] : ["addClass", "removeClass"]);
   var reachedLastItem = false;
   itms.some(function (itm, index) {
      var cond = (_removeClass ? $(itm).hasClass(className) : !$(itm).hasClass(className));
      if (cond) {
         if (nToggle > 0) {
            $(itm)[toggleClassFunction[0]](className);
            nToggle++;
         }
      } else {
         if (toggleFun != null) {
            if (toggleFun(index)) {
               $(itm)[toggleClassFunction[1]](className);
               nToggle++;
            }
            ;
         } else {
            $(itm)[toggleClassFunction[1]](className);
            nToggle++;
         }
      }
      return (nToggle === 2);
   });
   // Toggle 1st item again if toggled all in succession (valid when we have more than one items in the list)
   if (nToggle == 1 && itms.length > 1) {
      var itm = itms[0];
      var cond = (_removeClass ? $(itm).hasClass(className) : !$(itm).hasClass(className));
      if (cond) {
         $(itm)[toggleClassFunction[0]](className);
      } else {
         $(itm)[toggleClassFunction[1]](className);
      }
   }
}

// Get array of items from the DOM using a selector and id pattern
var getItems = function (className, id_pattern) {
   return $(className).toArray().filter(function (element) {
      var match;
      if($(element).attr("id")){
         match = ($(element).attr("id")).match(id_pattern);
      }else{
         match = null;
      }
      return (match == null ? false : true);
   });
}

// Get an array of items IDs' strings (starting with a #)
var idsToStrings = function (itemsSelector) {
   return $.map($(itemsSelector).toArray(), function (itm, i) {
      return "#" + $(itm).attr("id");
   });
}

// Called by "toggleNextClass". It is used for hiding the "Previous" button when we are at stage 1, i.e. there is no previous stage.
var nextStageIsFirstStage = function (lastStageItem) {
   $("#s1btnb").addClass("hidden");
}

// Reset styles based on source CSS (i.e. discard changes made through JS)
var resetCssStyles = function (selector) {
   $(selector).removeAttr("style");
}

// Self-explanatory
var setItemAttrColor = function (selector, attr, hexcolor) {
   var cssstr = $(selector).css(attr);
   if (cssstr.length == 0) { // FireFox (!)
      cssstr = $(selector).css(attr + "-top-color");
      $(selector).css(attr + "-top-color", cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
      cssstr = $(selector).css(attr + "-right-color");
      $(selector).css(attr + "-right-color", cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
      cssstr = $(selector).css(attr + "-bottom-color");
      $(selector).css(attr + "-bottom-color", cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
      cssstr = $(selector).css(attr + "-left-color");
      $(selector).css(attr + "-left-color", cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
   } else {
      $(selector).css(attr, cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g, hexcolor));
   }
}

var sectionSpinnerOn = false;
var sectionSpinnerText = "";

var toggleCurrentSectionSpinner = function() {
	if(sectionSpinnerOn)
	{
		$('.main_div .main_section:not(.hidden) h2').html(sectionSpinnerText);
	}
	else
	{
		sectionSpinnerText = $('.main_div .main_section:not(.hidden) h2').text();
		$('.main_div .main_section:not(.hidden) h2').html(sectionSpinnerText + " <i class='fa fa-cog fa-spin'></i>");
	}
	sectionSpinnerOn = !sectionSpinnerOn;
}

// Clear areas where results information appears.
var resetResultsInfoItms = function () {
   $("#server_feedback").empty();
   $("#dndres").empty();
   $("#results_p").html("Now analysing your data. Please wait for the results.");
}

var postFireUpAnalysisAndWait = function () {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   toggleCurrentSectionSpinner();
   $.ajax({
      url: cgi_bin_path + 'perform_analysis.php', //Server script to fire up data analysis
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false,
      beforeSend: function (jqXHR, settings) {
         //fire-up spinner
         //$("#server_feedback").html("<div class='loadingclockcontainer'><div class='box'><div class='clock'></div></div></div>");
         getRSS("http://www.nature.com/nmeth/current_issue/rss", "#server_feedback");
      },
   }).done(function (data, textStatus, jqXHR) {
   	toggleCurrentSectionSpinner();
      $("#server_feedback").empty();
      $("#s4btnf").prop('disabled', !data.success);
      if (data.success) {
         $("#results_p").html("Now you can inspect your results. When ready, click <em>Next</em>.");
         $("#dndres").html("<span><a href=" + data.results_url + "><strong>" + data.results_url.substr(data.results_url.lastIndexOf("\\") + 1) + "</strong></a></span>");
         patt = new RegExp("limma\-graphs");
         $.each(data.results_preview, function (idx, path_to_img_i)
         {
            var img_i = path_to_img_i.substr(data.results_url.lastIndexOf("\\") + 1);
            if (!patt.test(img_i)) {
               $("#server_feedback").append("<div class='resimg'><a href='" + path_to_img_i + "' target='_blank'><img src='" + path_to_img_i + "' width='120'></img></a></div>");
            }
         });
      } else {
         $("#results_p").html("");
         $("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>The analysis could not be completed: " + data.msg + "<em><strong></span>");
         if (data.R_dump.length > 0) {
            $("#server_feedback").append("<br><br><span style='font-family: Georgia; font-size: 95%; text-align: left; display: inline-block; width: 90%'><p>Please ensure that input parameters (such as number of replicates, number of biological conditions/labels etc) are correctly defined and input data format is valid. The statistical analysis routine relies heavily on the validity of the input parameters.</p><p>If the above does not apply, then the statistical analysis may have failed due to numerical problems (e.g. there were too many missing values/data points).</p><p> If you feel that none of the above is the case, please click <a href='mailto:msdiffexp@gmail.com?Subject=Session%20" + sessionid + "' target='_blank'><u>here</u></a> to notify via e-mail (do not delete the session id in the subject) the ProteoSign team for investigation of your analysis issue.</p></span>")
            //$("#server_feedback").append("<br><br><span style='font-family: \"Courier New\"'>Logged information:</span><br>");
            //$("#server_feedback").append("<div style='height: inherit'><textarea style='font-family: \"Courier New\"; font-size: 90%; width: 90%; height: 100%' readonly>"+ data.dump +"</textarea></div>");
         }
      }
   }).fail(function (jqXHR, textStatus, errorThrown) {
   	toggleCurrentSectionSpinner();
      $("#server_feedback").empty();
      $("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>An AJAX error occurred: " + errorThrown + "<em><strong></span>");
   });
}

var postClientServerClientInfo = function () {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   $.ajax({
      url: cgi_bin_path + 'get_serverclieninfo.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false,
      beforeSend: function (jqXHR, settings) {
      }}).done(function (data, textStatus, jqXHR) {
      clientname = data.hostname;
      softversion = data.version;
      $("#scrollingtext").html("Welcome <em>" + clientname + "</em> to ProteoSign");
      $("#proteosignversion").html("ProteoSign version " + softversion);
   }).fail(function (jqXHR, textStatus, errorThrown) {
   });
}

function dumpExpParamSQL(tmp){
   //Dump to a .sql the exp. params for DB import
   var sqlscript = [];
   var expdesc = $('input[name="expid"]').val();
   sqlscript.push("insert into dataset (desc) values ('" + expdesc + "');");
   var filesvaluesstr = $.map(uploadingFiles, function(i, v){return i.name;}).join("','");
   $.each(uploadingFiles, function (i, v){
      sqlscript.push("insert into files (file) values ('" + v.name + "');");
   });   
   var procfilesvaluesstr = $.map(rawfiles_structure, function(i, v){ return i.rawfile; }).join("','");
   $.each(rawfiles_structure, function (i, v){
      sqlscript.push("insert into processed_files (name) values ('" + v.rawfile + "');");
   });
   sqlscript.push("insert into dataset_files (dataset_id, file_id) select (select id from dataset where desc = '" + expdesc + "') as dataset_id, id as file_id from files where file in ('" + filesvaluesstr + "');");
   $.each(tmp, function (key, val){ 
      sqlscript.push("insert into param_value (param_id,value,dataset_id) select id,'" + val + "' as value,(select id from dataset where desc = '" + expdesc + "') as dataset_id from param where selector like '%" + key + "%';");
   });
   $.each(rawfiles_structure, function (key, val){ 
      sqlscript.push("insert into experimental_structure (processed_file_id,brep,trep,frac,dataset_id) values ('" + val.rawfile + "', '" + val.biorep + "', '" + val.techrep + "', '" + val.fraction + "', '0');");
   });
   sqlscript.push("update experimental_structure set dataset_id select id from dataset where dataset.desc = '" + expdesc + "' and experimental_structure.dataset_id = 0;");
   sqlscript.push("update experimental_structure set processed_file_id select id from processed_files where processed_files.name in ('" + procfilesvaluesstr + "') and experimental_structure.processed_file_id = 0;");
   //
   var link = document.getElementById('github');
   //link.href = URL.createObjectURL(new Blob([sqlscript.join("\n")], {type: 'text/plain'}));
   //link.click();
}

var postParameters = function (params) {
   var tmp = {};
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   $.each(params, function (idx, param_i)
   {
      var theval = $(param_i).val();
      switch ($(param_i).attr('type')) {
         case "checkbox":
            theval = ($(param_i).prop("checked") ? 1 : 0);
            // Here the on/off is transmitted Yes/No
            thedata.append($(param_i).attr('name'), (theval ? "T" : "F"));
            break;
         default:
            if (theval == null) {
               theval = '';
            }
            thedata.append($(param_i).attr('name'), theval);
            break;
      }
      if($(param_i).attr('id')){
         tmp[$(param_i).attr('id')] = theval;
      }else{
         tmp[$(param_i).attr('name')] = theval;
      }
   });
   dumpExpParamSQL(tmp);
   thedata.append("labelfree", ((peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) ? 'T' : 'F'));
   thedata.append("exp_struct", gen_expdesign(rawfiles_structure));
   toggleCurrentSectionSpinner();
   $.ajax({
      url: cgi_bin_path + 'upload_parameters.php', //Server script to receive parameters
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false
   }).done(function (data, textStatus, jqXHR) {
   	toggleCurrentSectionSpinner();
//if there was a server-side error alert.
      if (!data.success) {
         alert("ERROR on SERVER: " + data.msg);
      } else {
         postFireUpAnalysisAndWait();
      }
   });
}

// Executed when a "Next" type of button is clicked
var executeStage = function (stageIndex) {
   var ret = true;
   switch (stageIndex) {
      case 1:
         if(tour_mode){
            onShowDialog("#tourModeDlg1");
         }
         break;
      case 2:
         if(tour_mode){
            onShowDialog("#tourModeDlg2");
         }
         break;
      case 3:
         var parameterObjs = $("#s3expparams input,#s3expparams select");
         if (validateParameters(parameterObjs)) {
            postParameters(parameterObjs);
            // Clear areas where results information appears.
            resetResultsInfoItms();
         } else {
            ret = false;
         }
         break;
      case 5:
         resetState();
         break;
      default:
   }

   return ret;
}
// Executed when a "Back" type of button is clicked
var rollbackStage = function (stageIndex) {
   var ret = true;
   stageIndex = getItems("button.main", /s[0-9]+btnb/).length - stageIndex - 1;
   switch (stageIndex) {
      case 1:
         resetState();
      default:
   }   
   return ret;
}

var unsuccessfullyUploadedFiles = {};
var uploadEfforts = {};
var peptideLabelsFromFile = [];
var peptideLabelsNamesFromFile = [];
var peptideLabelsFromFileCombs = [];
var nFormLabels = 1;
var nToUpload;
var debug_ajax_data;
var tour_mode = false;
// Return false if at least one required parameter (existence of data-required attr) is not set (empty text field)
// Mark invalid parameters with a red border color (currently tested on input fields)
// Marked fields are reset through the 'focusout' event (see respective binding in function document.ready)
var validateParameters = function (params) {
   var nValid = 0;
   var nInvalid = 0;
   $.each(params, function (idx, param_i)
   {
      switch ($(param_i).attr('type')) {
         case "text":
            if ($(param_i).attr('data-required') != null && $(param_i).attr('data-required')) {
               if ($(param_i).val() == "" && !$(param_i).hasClass("hidden")) {
                  nInvalid++;
                  setItemAttrColor(param_i, "border", "#E60000");
               }
            }
            break;
         default:
            nValid++;
            break;
      }
   });
   return nInvalid == 0;
}

// reset "counters"/states
var resetState = function (uploading_new_file = false) {
   unsuccessfullyUploadedFiles = {};
   uploadEfforts = {};
   if (!$("#explbl1definition").hasClass("hidden")) {
      $("#explbl1definition").addClass("hidden");
   }
	if(uploading_new_file == false)
	{
		// Reset items that contained information from previous data input files (e.g. label information)
		$("#s3expparamsDlgLabelsSelection").empty();
		$("#explbl1name_").empty();
		$("#explbl0name_").empty();
		$("#s3advparams select[name='expquantfiltlbl']").empty();
		$("#quantsoftdetectedspan").empty();
		rawfiles = undefined;
		procProgram = "";
		peptideLabelsFromFile = [];
		peptideLabelsFromFileCombs = [];
		peptideLabelsNamesFromFile = [];
		$("#s2uluploaders > table").empty();
		sessionid = new Date().getTime();
		uploaded_files = 0;
		upload_was_successful = false;
		$("#s2btnf").prop('disabled', true);
		types_of_files_uploaded = [];
		nToUpload = 0;
	}
	
   nUploaded = uploaded_files;
   tour_mode = false;
}

// "uploadingFiles": Files (array returned by FileChooser) selected for upload in stage #2
var uploadFiles = function (serverSide, postSuccess) {
   resetState(true);
   tour_mode = serverSide;
   $("#s2btnf").prop('disabled', true);

   $.each(uploadingFiles, function (idx, file_i) {
	  var temp_index = uploaded_files + nToUpload + idx;
      $("#s2uluploaders table").append("<tr><td>" + file_i.toString() + "</td><td><progress max='100' value='0' id=uploadfile" + temp_index + "><div class='progress-bar'><span style='width: 80%;'></span></div></progress></td></tr>");
	  // console.log("	created table row for file: " + temp_index + file_i.toString());
   });
	// fire AJAX calls
   $.each(uploadingFiles, function (idx, file_i)
   {
	  var temp_index = uploaded_files + nToUpload;
      postFile(temp_index, file_i, serverSide, postSuccess);
	  nToUpload++;
	  tmp_nToUpload++;
	  // console.log("	fired post file for file: " + temp_index);
   });
  }

// Called by respective AJAX event handlers (see postFile function below)
var uploadFinished = function (success, idx, file) {
	// console.log("	Upload Finished called for idx: " + idx + "nToUpload: " + nToUpload);
	nToUpload = nToUpload - 1;
	uploaded_files++;
	// console.log("	uploaded files updated: " + uploaded_files);
   uploadEfforts[idx] = file;
   if (!success) {
	  // console.log("unsuccessfullyUploadedFiles != 0");
      unsuccessfullyUploadedFiles[idx] = file;
   }
}

// jQuery AJAX POST for uploading a single file
var postFile = function (idx, file, serverSide, postSuccess) {
   var thedata = new FormData();
   thedata.append('thefile', file);
   thedata.append('session_id', sessionid);
   thedata.append('server_side', serverSide);
   var progresstditm = $("#s2uluploaders table tr:nth-child(" + (idx + 1) + ") td").get(1);
   $.ajax({
      url: cgi_bin_path + 'upload_files.php', //Server script to receive file
      type: 'POST',
      xhr: function () {  // Custom XMLHttpRequest
         var myXhr = $.ajaxSettings.xhr();
         if (myXhr.upload) { // Check if upload property exists
            myXhr.upload.addEventListener('progress', function (e) {
               if (e.lengthComputable) {
                  if (e.loaded / e.total == 1.0) {
                     $(progresstditm).html("<span class='uploadSuccessMsg'><strong><em>Processing, please wait ...<em><strong></span>");
                     toggleCurrentSectionSpinner();
                  } else {
                     helper_setItemAttr("#uploadfile" + idx, {value: e.loaded, max: e.total});
					 // console.log("#uploadfile" + idx + " new prog: " + e.loaded + "/"  +  e.total);
					 // console.log("		#uploadfile" + idx + "value " + $("#uploadfile" + idx).attr("value") + "max: " + $("#uploadfile" + idx).attr("max"));
                  }
               }
            }, false); // For handling the progress of the upload
         }
         return myXhr;
      },
      // Form data
      data: thedata,
      //Options to tell jQuery not to process data or worry about content-type.
      cache: false,
      contentType: false,
      processData: false,
      //Ajax events
      beforeSend: function (jqXHR, settings) {
         if (serverSide) {
            helper_setItemAttr("#uploadfile" + idx, {value: 0, max: 100});
         }
      }}).done(function (data, textStatus, jqXHR) {
      	if(sectionSpinnerOn)
	      	toggleCurrentSectionSpinner();
//debug_ajax_data = data;
//If server-side everything went fine (internal things that the server had to do with the client's file, such as storage etc)
      uploadFinished(data.success, idx, file);
      //if everything went fine enable button for next stage and print OK. Just print the error message otherwise.
      if (data.success) {
types_of_files_uploaded.push(data.file_type);
		  if (data.msg == "")
		  {
			$(progresstditm).html("<span class='uploadSuccessMsg'><strong><em>OK<em><strong></span>");
		  }
		  else
		  {
			$(progresstditm).html("<span class='uploadWarningMsg'><strong><em>" + data.msg + "<em><strong></span>");
		  }
		  var tmp_rawfiles = [];
         for (var i = 0; i < data.raw_filesnames.length; i++) {
            tmp_rawfiles.push(data.raw_filesnames[i]);
         }
// If we have info regarding rawdata files names, save it					
         if (tmp_rawfiles.length > 0) {
            rawfiles = tmp_rawfiles;
            reset_reps();
         }
// If we have info regarding peptide labels and such info has not been available in previous upload
         if (peptideLabelsNamesFromFile.length == 0 && (data.peptide_labels_names.length > 0 || data.peptide_labels.length > 0)) {
            peptideLabelsFromFile = data.peptide_labels.sort();
            peptideLabelsNamesFromFile = data.peptide_labels_names.sort();
            // some doc items that depend on whether the experiment is label-free or not
            // 1. Non-labelled "background" species present?
            var item1 = $("#s3advparams input[name='explbl00']").closest("tr").children().first();
            // 2. Quantitation filtering?
            var item2 = $("#s3advparams input[name='expquantfilt']").closest("tr").children().first();
            // 3. Species label #1
            var item3 = $("#explbl1name_").closest("tr").children().first();
            // 4. Species label #1 select item td
            var item4 = $("#explbl1name_").closest("td");
            // 5. Label tooltip
            var item5 = $("#quantsoftdetectedspan").closest("td").children().first();
            // 6. Species label #1 definition
            var item6 = $("#explbl1definition");
            // if not label-free data
            if (peptideLabelsNamesFromFile.length > 0) {
// Set states of parameters relevant to labelled experiments accordingly
               $(item1).prop('disabled', false);
               $(item2).prop('disabled', false);
               $(item3).html('&#8212 Species label #1');
               $(item4).html('<select data-required="true" id="explbl1name_" name="explbl1name" type="text" placeholder="Name"></select>');
               // Only Proteome Discoverer data currently provide label definition information.
               // When this information is made available it means that our data originate from PD software.
               $("#s3expparams input[name='exppddata']").prop('checked', peptideLabelsFromFile.length > 0);
               //<img class="callout" src="../images/callout_black.gif" /><strong>Warning!</strong><br><u>The order of labels defined here matters</u>. Define your labels in the same order they were defined in <a id="quantsoftdetectedspan"></a>. If there exist unlabeled species, please define them in the <em>advanced parameters</em> section below.
               $("#quantsoftdetectedspan").text(peptideLabelsFromFile.length > 0 ? "Proteome Discoverer" : "MaxQuant");
               $(item5).removeClass('hidden');
               $(item6).attr('placeholder', 'Definition');
            } else {
//label-free case (disable non-applicable parameters and rename others accordingly)
               $(item1).prop('disabled', true);
               $(item2).prop('disabled', true);
               $(item3).html('&#8212 Biological condition #1');
               $(item4).html('<input data-required="true" id="explbl1name_" name="explbl1name" type="text" onkeypress="inputChCheck(event,\'^(?!_)[a-zA-Z0-9_]+$\',20)" placeholder="Character rules apply"></input>');
               $(item5).addClass('hidden');
               $(item6).attr('placeholder', 'Raw file');
            }

            $.each(peptideLabelsFromFile, function (idx, lbl_i) {
               $("#s3expparamsDlgLabelsSelection").append("<option value='" + lbl_i + "'>" + lbl_i + "</option>");
            });
            $.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
               $("#explbl1name_").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
               $("#explbl0name_").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
               $("#s3advparams select[name='expquantfiltlbl']").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
            });
            if (peptideLabelsFromFile.length > 0) {
               $("#explbl1definition").removeClass("hidden");
            }
            while (nFormLabels > 1) {
               removeFormLabel();
            }
         }
      } else {
         $(progresstditm).html("<span class='uploadErrorMsg'><strong><em>A server-side error occurred: " + data.msg + "<em><strong></span>");
      }
      if (typeof (postSuccess) == "function") {
         postSuccess();
      }
   }).fail(function (jqXHR, textStatus, errorThrown) {
   	if(sectionSpinnerOn)
	   	toggleCurrentSectionSpinner();
      $(progresstditm).empty();
      $(progresstditm).html("<span class='uploadErrorMsg'><strong><em>An AJAX error occurred: " + errorThrown + "<em><strong></span>");
      uploadFinished(false, idx, file);
   });
}

var bind_explbldefinition_focus = function (explbldefinition) {
   $(explbldefinition).on("focus", function () {
      $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($("#s3expparamsDlgLabels").width() / 2)});
      $('body').append('<div id="mask"></div>');
      $("#s3expparamsDlgLabels").fadeIn(300);
      //$('#mask').fadeIn(300);
      var lbl = $(this);
      $("#dlglabelsBtnOK").unbind();
      $("#dlglabelsBtnOK").on("click", function () {
         dlgFadeout();
         var selection = $("#s3expparamsDlgLabelsSelection").val();
         if (selection != null) {
            $(lbl).val(selection.join(","));
         }
      });
   });
}

var onShowDialog = function (selector){
   $(".expparamsDlg").css({"left": ($("body").width() / 2) - ($(selector).width() / 2)});
   $('body').append('<div id="mask"></div>');
   $(selector).fadeIn(300);
   //$('#mask').fadeIn(300);   
}

var addFormLabel = function () {
   if (!$("#explbl" + nFormLabels + "definition").hasClass("hidden") && $("#explbl" + nFormLabels + "definition").val().length == 0) {
      return;
   }
   nFormLabels++;
   var last_tr_of_table = $("#btnAddLabel").closest("table").children().first().children().last();
   if (peptideLabelsNamesFromFile.length > 0) {
      $(last_tr_of_table).after("<tr><td>&#8212 Species label #" + nFormLabels + "</td><td></td><td><select data-required='true' id='explbl" + nFormLabels + "name_' name='explbl" + nFormLabels + "name'></select></td><td><input data-required='true' id='explbl" + nFormLabels + "definition' name='explbl" + nFormLabels + "def' type='text' placeholder='Definition' readonly" + (peptideLabelsFromFile.length > 0 ? "" : " class='hidden'") + "/></td></tr>");
      if (peptideLabelsFromFile.length > 0) {
         bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
         $("#explbl" + (nFormLabels - 1) + "definition").off("focus");
      }
      $.each(peptideLabelsNamesFromFile, function (idx, lblname_i) {
         $("#explbl" + nFormLabels + "name_").append("<option value='" + lblname_i + "'>" + lblname_i + "</option>");
      });
   } else {
//label-free case
      if (peptideLabelsFromFile.length > 0) {
         $(last_tr_of_table).after("<tr><td>&#8212 Biological condition #" + nFormLabels + "</td><td></td><td><input data-required='true' id='explbl" + nFormLabels + "name_' name='explbl" + nFormLabels + "name' type='text' onkeypress='inputChCheck(event,\"^(?!_)[a-zA-Z0-9_]+$\",20)' placeholder='Character rules apply'></input></td><td><input data-required='true' id='explbl" + nFormLabels + "definition' name='explbl" + nFormLabels + "def' type='text' placeholder='Raw file' readonly" + (peptideLabelsFromFile.length > 0 ? "" : " class='hidden'") + "/></td></tr>");
         bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
         $("#explbl" + (nFormLabels - 1) + "definition").off("focus");
      }
   }
   var addbtntd = $("#btnAddLabel").closest("td");
   var rembtntd = $(addbtntd).next();
   last_tr_of_table = $(last_tr_of_table).next();
   $(last_tr_of_table).children().last().after($(addbtntd));
   $(last_tr_of_table).children().last().after($(rembtntd));
   $("#btnRemLabel").prop("disabled", false);
   // label-free case
   if (peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) {
      $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsFromFile.length - 2) : peptideLabelsFromFile.length - 1));
   } else {
      $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsNamesFromFile.length - 2) : peptideLabelsNamesFromFile.length - 1));
   }
}

var removeFormLabel = function () {
   nFormLabels--;
   var rembtntd = $("#btnRemLabel").closest("td");
   var addbtntd = $(rembtntd).prev();
   var last_tr_of_table = $("#btnRemLabel").closest("table").children().first().children().last();
   $(last_tr_of_table).prev().children().last().after($(addbtntd));
   $(last_tr_of_table).prev().children().last().after($(rembtntd));
   $(last_tr_of_table).remove();
   $("#btnRemLabel").prop("disabled", nFormLabels == 1);
   // label-free case
   if (peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) {
      $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsFromFile.length - 2) : peptideLabelsFromFile.length - 1));
   } else {
      $("#btnAddLabel").prop("disabled", nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsNamesFromFile.length - 2) : peptideLabelsNamesFromFile.length - 1));
   }
   if (peptideLabelsFromFile.length > 0) {
      bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
   }
}

var downloadTestDataset = function (dataset_desc) {
   window.location = cgi_bin_path + 'download_test_data.php?session_id=' + sessionid + '&dataset_info_requested=' + dataset_desc;
}

var postTestDatasetInfo = function (dataset_desc) {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   thedata.append('descriptions_requested', false);
   thedata.append('dataset_info_requested', dataset_desc);
   $.ajax({
      url: cgi_bin_path + 'load_test_data.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false
   }).done(function (data, textStatus, jqXHR) {
//if there was a server-side error alert.
      if (!data.success) {
         alert("ERROR on SERVER: " + data.msg);
      } else {
         uploadingFiles = data.queryres.file;
         if (uploadingFiles && uploadingFiles.length > 0) {
//Start uploading ...
            uploadFiles(true, function () {
               if (++nUploaded < uploadingFiles.length) {
                  return;
               }
               $("#s2btnf").triggerHandler("click");
               $("#s3showhideadvparams").trigger("click");
               $("input[name='expid']").val(dataset_desc.replace(/[^a-zA-Z0-9]+/g, "_"));
               $.each(data.queryres.selector, function (idx, param_selector)
               {
                  // console.log(param_selector + " = " + data.queryres.value[idx]);
                  switch ($(param_selector).attr('type')) {
                     case "checkbox":
                        // Here the 0/1 is transmitted false/true
                        var theval = (data.queryres.value[idx] == "0" ? false : true);
                        if ($(param_selector).prop("checked") != theval) {
                           $(param_selector).trigger("click");
                        }
                        break;
                     default:
                        m = param_selector.match(/^#explbl[2-9]+[0-9]*name_$/);
                        if (m != null && !$(param_selector).is(':visible')) {
						   // console.log(m);
                           addFormLabel();
                        }
                        $(param_selector).val(data.queryres.value[idx]);
                        break;
                  }
               });
               for (var i = 0; i < data.queryres.raw_file.length; i++) {
                  rawfiles_structure.push({rawfile: data.queryres.raw_file[i], biorep: data.queryres.brep[i], techrep: data.queryres.trep[i], fraction: data.queryres.frac[i]});
                  var tds = $('tr[id="tr_' + data.queryres.raw_file[i] + '"] td');
                  $(tds[1]).text(data.queryres.brep[i] == 0 ? '-' : data.queryres.brep[i]);
                  $(tds[2]).text(data.queryres.trep[i] == 0 ? '-' : data.queryres.trep[i]);
                  $(tds[3]).text(data.queryres.frac[i] == 0 ? '-' : data.queryres.frac[i]);
               }
               set_reps();
               $("#s3expparams").animate({scrollTop: 0}, "slow");
            });
         } else {
            $("#s2btnf").prop('disabled', true);
         }
      }
   }).fail(function (jqXHR, textStatus, errorThrown) {
      alert("An AJAX error occurred: " + errorThrown);
   });
}

var postTestDatasetsInfo = function () {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   thedata.append('descriptions_requested', true);
   $.ajax({
      url: cgi_bin_path + 'load_test_data.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false
   }).done(function (data, textStatus, jqXHR) {
//if there was a server-side error alert.
      if (!data.success) {
         alert("ERROR on SERVER: " + data.msg);
      } else if(data.queryres.desc) {
         var i = 1;
         $.each(data.queryres.desc, function (idx, dataset_desc)
         {
            $("#s1TestDatasetsSelection").append("<option value='" + (i++) + "'>" + dataset_desc + "</option>");
         });
      }
   }).fail(function (jqXHR, textStatus, errorThrown) {
      alert("An AJAX error occurred: " + errorThrown);
   });
}

var bioreps;
var techreps;
var fractions;
var rawfiles;
var rawfiles_structure;
var rep_counts;
var lastclicked_rawfiles_tbl_tr = null;
var reset_reps = function () {
   bioreps = 0;
   techreps = 0;
   fractions = 0;
   rawfiles_structure = [];
   rep_counts = {biorep: []};
   rawfiles_tbl_allfiles_DT.clear();
   $.each(rawfiles, function (idx, filename_i) {
      rawfiles_tbl_allfiles_DT.row.add(
              {
                 'fname': filename_i,
                 'brep': '-',
                 'trep': '-',
                 'frac': '-',
                 'DT_RowClass': "rawfiles_tbl_td_not_selected",
                 'DT_RowId': 'tr_' + filename_i
              }
      );
   });
   rawfiles_tbl_allfiles_DT.draw();
   $('#rawfiles_tbl_allfiles tbody tr').click(function (event) {
      if (event.shiftKey) {
         if (lastclicked_rawfiles_tbl_tr !== null) {
            var i1 = $('#rawfiles_tbl_allfiles tbody tr').index(lastclicked_rawfiles_tbl_tr);
            var i2 = $('#rawfiles_tbl_allfiles tbody tr').index(this);
            var trs = $('#rawfiles_tbl_allfiles tbody tr');
            if (i2 > i1) {
               for (var i = (i1 + 1); i <= i2; i++) {
                  $(trs[i]).toggleClass('rawfiles_tbl_td_selected');
               }
            } else {
               for (var i = (i1 - 1); i >= i2; i--) {
                  $(trs[i]).toggleClass('rawfiles_tbl_td_selected');
               }
            }
         }
      } else {
         $(this).toggleClass('rawfiles_tbl_td_selected');
      }
      lastclicked_rawfiles_tbl_tr = this;
   });
}


function set_reps() {
   for (var i = 0; i < rawfiles_structure.length; i++) {
      var rep = "biorep";
      var max_level = 0;
      if (rawfiles_structure[i][rep] != 0) {
         if (!(rawfiles_structure[i][rep] in rep_counts[rep])) {
            rep_counts[rep][rawfiles_structure[i][rep]] = {techrep: []};
            bioreps++;
         }
      }
      rep = "techrep";
      if (rawfiles_structure[i][rep] != 0) {
         if (!(rawfiles_structure[i][rep] in rep_counts["biorep"][rawfiles_structure[i]["biorep"]][rep])) {
            rep_counts["biorep"][rawfiles_structure[i]["biorep"]][rep][rawfiles_structure[i][rep]] = {fraction: []};
            techreps++;
         }
         max_level = 1;
      }
      rep = "fraction";
      if (rawfiles_structure[i][rep] != 0) {
         if (!(rawfiles_structure[i][rep] in rep_counts["biorep"][rawfiles_structure[i]["biorep"]]["techrep"][rawfiles_structure[i]["techrep"]]["fraction"])) {
            rep_counts["biorep"][rawfiles_structure[i]["biorep"]]["techrep"][rawfiles_structure[i]["techrep"]]["fraction"][rawfiles_structure[i][rep]] = 1;
            fractions++;
         }
         max_level = 2;
      }
   }
//console.log(rep_counts);
//console.log([bioreps, techreps, fractions]);
   if (rawfiles_structure.length == rawfiles.length) {	//if all files have been assigned something
      $("#s22btnf").prop('disabled', !(rawfiles.length > 0 && rawfiles_structure.length == rawfiles.length));
   }
   $("#btnResetExpStructCoord").prop('disabled', rawfiles_structure.length == 0);
   

}

var gen_expdesign = function (struct) {
   var ret = "";
   for (var i = 0; i < struct.length; i++) {
      ret = ret + struct[i].rawfile + "\t" + struct[i].biorep + "\t" + struct[i].techrep + "\t" + struct[i].fraction + "\n";
   }
   return ret;
}

var rss_i;
var renderRSSData = function (data, renderelem) {
   var prev_html = "<notset>";
   rss_i = 0;
   var items = Object.keys(data);
   items = shuffle(items);
   var updateFun = function () {
      // If the current html content of renderelem is not the same as the last one set by this function, it means some other function has set the html content so we have to terminate this infinite update of renderelem's content by not calling the setTimeout function (essentially calling oneself) and just returning.
      if (prev_html.localeCompare("<notset>") != 0 && $(renderelem).html().localeCompare(prev_html) != 0) {
         return;
      }
      if (rss_i == (items.length - 1)) {
         rss_i = 0;
      }
      var item = items[rss_i++];
      //console.log(data[item]);
      if (!/^http/.test(data[item]) || /gif|png|jpg|jpeg$/.test(data[item])) {
         inter = setTimeout(updateFun, 100);
         return;
      }
      $(renderelem).hide();
      prev_html = '<p>Inside the current issue of Nature Methods:<br><br><a href="' + data[item] + '" target="_blank"><strong>' + item + '</strong></a></p>';
      //$("#rsscontent").html('<p><a href="'+data[item]+'" target="_blank">'+item+'</a></p><br><iframe src="'+data[item]+'" style="display: block; width:100%; height:100%;"/>').fadeIn(300);
      $(renderelem).html(prev_html).fadeIn(300);
      var intertime = (Math.log((item.split(/\s/).length) + 1) / Math.log(1.6)) * 1000 + 1000;
      //console.log(item.split(/\s/).length + ': ' + intertime);
      inter = setTimeout(updateFun, intertime);
   }
   var inter = setTimeout(updateFun, 100);
}

var getRSS = function (rssurl, renderelem) {
   var thedata = new FormData();
   thedata.append('session_id', sessionid);
   thedata.append('rssurl', rssurl);
   $.ajax({
      url: cgi_bin_path + 'get_rss.php',
      type: 'POST',
      // Form data
      data: thedata,
      //Options to tell jQuery not to worry about content-type.
      processData: false,
      cache: false,
      contentType: false,
      beforeSend: function (jqXHR, settings) {
      }
   }).done(function (data, textStatus, jqXHR) {
      renderRSSData(data, renderelem);
   }).fail(function (jqXHR, textStatus, errorThrown) {
   });
}

var dlgFadeout = function () {
   $(".expparamsDlg").fadeOut(300, function () {
      $('#mask').remove();
   });
}


$(document).ready(function () {
   var forward_buttons = getItems("button.main", /s[0-9]+btnf/);
   var backward_buttons = getItems("button.main", /s[0-9]+btnb/);
   // Binds the click event to "toggleNextClass" for each "forward button" (button with class button.main and id /s[0-9]+btnf/) 
   forward_buttons.forEach(function (btn) {
      $(btn).on("click", function () {
         toggleNextClass(idsToStrings(".main_div .main_section"), "hidden", true, executeStage);
         if ($(backward_buttons[0]).hasClass("hidden")) {
            $(backward_buttons[0]).removeClass("hidden");
         }
		rawfiles_tbl_allfiles_DT.columns.adjust().draw();
   });
   });
   // Binds the click event to "toggleNextClass" for each "backward button" (button with class button.main and id /s[0-9]+btnb/) 
   backward_buttons.forEach(function (btn) {
      $(btn).on("click", function () {
         toggleNextClass(idsToStrings(".main_div .main_section").reverse(), "hidden", true, rollbackStage);
		 rawfiles_tbl_allfiles_DT.columns.adjust().draw();
      });
   });
   // Bind event for file upload button
   $("#s2btnupld").on("click", function () {
      $("#__s2btnupld").click();
   });
   // Bind event when file(s) is/are chosen
   $("#__s2btnupld").change(function () {
      var fnames = "";
       //$("#s2uluploaders > table").empty();
	   // console.log("files: " + this.files.length)
      if (this.files.length > 0) {
//Start uploading ...
         uploadingFiles = this.files;
			if (window.File && window.FileReader && window.FileList && window.Blob) {
				oversized_files_idxs = [];
		      $.each(uploadingFiles, function (idx, file_i)
		      {
		      	if(file_i.size > 2147483648)
				// if(false)
		      	{
		      		oversized_files_idxs.push(idx);
		      	}
		      });				
			}
			if(oversized_files_idxs.length == 0)
			{
		      uploadFiles(false, function () {
				 // console.log("upload files report: nuploaded / uploaded_files / typeof rawfiles / peptideLabelsNamesFromFile.length / peptideLabelsFromFile.length : " + nUploaded + "/" + uploaded_files + "/" + typeof rawfiles + "/" + peptideLabelsNamesFromFile.length + "/" + peptideLabelsFromFile.length);
				 //Check what type of files has the user uploaded:
				 var Files_valid = false;
				 if (types_of_files_uploaded.indexOf("MQ") != -1 && types_of_files_uploaded.indexOf("MQP") != -1)
				 {
					 if(types_of_files_uploaded.indexOf("PD") == -1)
					 {
						 //Maxquant valid files:
						 Files_valid = true;
						 procProgram = "MQ";
						 $("#exppddata").prop("checked", false);
					 }
					 else
					 {
						procProgram = "";
						//Both MQ and PD files were uploaded:
						alert("WARNING: Some of the files you uploaded are processed by MaxQuant and some by Proteome Discoverer, we are sorry to inform you that it is impossible to proceed.");
						$("#s2btnf").prop('disabled', true);
						Files_valid = false;
					 }
				 }
				 if(types_of_files_uploaded.indexOf("PD") != -1)
				 {
					 if (types_of_files_uploaded.indexOf("MQ") == -1 && types_of_files_uploaded.indexOf("MQP") == -1)
					 {
						 //PD valid files:
						 Files_valid = true;
						 procProgram = "PD";
						 $("#exppddata").prop("checked", false);
					 }
					 //No need to check if both MQ and PD files were uploaded, this has already been done
				 }
				 //If the user has uploaded more than one file from the same type alert them:
				if ((procProgram == "MQ" && types_of_files_uploaded.filter(filterFunction).length > 2) 	|| (procProgram == "PD" && types_of_files_uploaded.filter(filterFunction).length > 1))
				{
					alert("WARNING: You uploaded too many files from " + procProgram + "! Please make sure you upload MultiConsensus files. Please refresh the current page and try again.");
					$("#s2btnf").prop('disabled', true);
					Files_valid = false;
				}
				 // console.log("Files valid" + Files_valid);
				 // console.log("	Check to enable next: nToUpload = " + nToUpload);
				 if (Files_valid == true && (upload_was_successful == true || (nToUpload == 0 && (peptideLabelsNamesFromFile.length > 0 || peptideLabelsFromFile.length > 0)))) {
					upload_was_successful = true;
		            $("#s2btnf").prop('disabled', false);
		         }
		      });			
			}
			else
			{
				alert("WARNING: At least one of the files chosen is oversized, we are sorry to inform you that it is impossible to proceed.");
	         $("#s2btnf").prop('disabled', true);
			}
      } else {
         //$("#s2btnf").prop('disabled', true);
      }
   });
   //Toggle visibility of table with advanced params
   $("#s3showhideadvparams").on("click", function () {
      var txt = $(this).text();
      if ($("#s3advparams").hasClass("hidden")) {
         $("#s3advparams").removeClass("hidden");
         $(this).text(txt.replace("Show", "Hide"));
         var parentdiv = $(this).closest("div");
         parentdiv.scrollTop(parentdiv.prop("scrollHeight"));
      } else {
         $("#s3advparams").addClass("hidden");
         $(this).text(txt.replace("Hide", "Show"));
      }
   });
   //Add functionality of add/remove labels buttons
   $("#btnAddLabel").on("click", addFormLabel);
   $("#btnRemLabel").on("click", removeFormLabel);
   bind_explbldefinition_focus("#explbl" + nFormLabels + "definition");
   $("#dlglabelsBtnCancel").on("click", function () {
      $(".expparamsDlg").fadeOut(300, function () {
         $('#mask').remove();
      });
   });
   $("#dlglabelsBtnInvertSel").on("click", function () {
      var tmp = $("#s3expparamsDlgLabelsSelection option").not(".hidden").not(":selected");
      $("#s3expparamsDlgLabelsSelection option").not(".hidden").filter(":selected").prop('selected','');
      $(tmp).prop('selected','selected');
   });
   //Toggle visibility on inputs that are dependent to each other
   // #1: dependency between quantitation filtering settings
   $("#s3advparams input[name='expquantfilt']").on("click", function () {
      if ($(this).prop('checked')) {
         $("#s3advparams select[name='expquantfiltlbl']").closest("tr").removeClass("hidden");
         $("#s3advparams input[name='expquantfiltprot']").closest("tr").removeClass("hidden");
         var parentdiv = $(this).closest("div");
         parentdiv.scrollTop(parentdiv.prop("scrollHeight"));
      } else {
         $("#s3advparams select[name='expquantfiltlbl']").closest("tr").addClass("hidden");
         $("#s3advparams input[name='expquantfiltprot']").closest("tr").addClass("hidden");
      }
      ;
   });
   // #2: dependency between PD data and label definitions.
   $("#s3expparams input[name='exppddata']").on("click", function () {
   });
   //TODO
   // #3: dependency between background existence and ?
   $("#s3advparams input[name='explbl00']").on("click", function () {
      if ($(this).prop('checked')) {
         $("#s3advparams select[name='explbl0']").closest("tr").removeClass("hidden");
         if (nFormLabels > peptideLabelsNamesFromFile.length - 1) {
            removeFormLabel();
         }
         var parentdiv = $(this).closest("div");
         parentdiv.scrollTop(parentdiv.prop("scrollHeight"));
      } else {
         $("#s3expparams select[name='explbl0']").closest("tr").addClass("hidden");
      }
   });
   // TextField validation
   $("#s3expparams input[name='expbioreps']").on("focusout", function () {
      if ($(this).val() != "" && $("#s3expparams input[name='exptechreps']").val() != "" && (Number($(this).val()) + Number($("#s3expparams input[name='exptechreps']").val()) < 3)) {
         $(this).focus().select();
      }
   });
   $("#s3expparams input[name='exptechreps']").on("focusout", function () {
      if ($(this).val() != "" && $("#s3expparams input[name='expbioreps']").val() != "" && (Number($(this).val()) + Number($("#s3expparams input[name='expbioreps']").val()) < 3)) {
         $(this).focus().select();
      }
   });
   // Needed after form validation (because some required fields might have been highlighted)
   $("#s3expparams input[data-required]").on("focusout", function () {
      resetCssStyles(this);
   });
   // Display chopped long raw file name on a tooltip
   $('#rawfiles_tbl_allfiles').delegate('td', 'mouseenter', function () {
      var $this = $(this);
      if (this.offsetWidth < this.scrollWidth && !$this.attr('title')) {
         $this.attr('title', $this.text());
      }
   });
   //

   $('#btnAssignExpStructCoord').on("click", function () {
      var items = $('#rawfiles_tbl_allfiles').find('.rawfiles_tbl_td_selected');
      var def_biorep = Number($('#expstructcoord_biorep').val());
      var def_techrep = Number($('#expstructcoord_techrep').val());
      var curr_biorep = def_biorep;
      var curr_techrep = def_techrep;
      var curr_fraction = 0;
      var rep_offset = 1;
      if (def_biorep > 0 && def_biorep > (bioreps + 1)) {	// return if user tries to assign biorep X without having defined biorep X-1
         return;
      }
      if (def_techrep > 0 && def_techrep > (techreps + 1)) {	// same as above but for techreps
         return;
      }
      $("#btnResetExpStructCoord").prop('disabled', false);
      for (var i = 0; i < items.length; i++) {
         var items_tds = $(items[i]).find('td');
         var items_biorep = items_tds[1];
         var items_techrep = items_tds[2];
         var items_frac = items_tds[3];
         if ($(items_biorep).text() == '-' && $(items_techrep).text() == '-' && $(items_frac).text() == '-') {
            if (def_biorep > 0) { // i.e. non-blank
               if (def_techrep == 0) { // blank
//techreps, from current count to items.length, except we have label-free data (peptideLabelsNamesFromFile.length == 0)
                  if (def_biorep in rep_counts["biorep"] && peptideLabelsNamesFromFile.length > 0) {
                     curr_techrep = (rep_counts["biorep"][def_biorep].techrep.length - 1) + rep_offset++;
                  } else {
                     curr_techrep = rep_offset++;
                  }
               } else {
// fractions, from current count to items.length, except we have label-free data (peptideLabelsNamesFromFile.length == 0)
                  if (def_biorep in rep_counts["biorep"] && def_techrep in rep_counts["biorep"][def_biorep].techrep && peptideLabelsNamesFromFile.length > 0) {
                     curr_fraction = (rep_counts["biorep"][def_biorep].techrep[def_techrep].fraction.length - 1) + rep_offset++;
                  } else {
                     curr_fraction = rep_offset++;
                  }
               }
            } else {
               if (def_techrep == 0) { // blank
// bioreps, from current count to items.length, except we have label-free data (peptideLabelsNamesFromFile.length == 0)
                  if (peptideLabelsNamesFromFile.length > 0) {
                     curr_biorep = rep_counts["biorep"].length + rep_offset++;
                  } else {
                     curr_biorep = rep_offset++;
                  }
               } else {
// error, a biorep must be specified
                  if (rawfiles_structure.length == 0) {
                     $("#btnResetExpStructCoord").prop('disabled', true);
                  }
                  return;
               }
            }
            rawfiles_structure.push({rawfile: $(items_tds[0]).text(), biorep: curr_biorep, techrep: curr_techrep, fraction: curr_fraction});
            $(items_biorep).text(curr_biorep == 0 ? '-' : curr_biorep);
            $(items_techrep).text(curr_techrep == 0 ? '-' : curr_techrep);
            $(items_frac).text(curr_fraction == 0 ? '-' : curr_fraction);
         }
      }
      set_reps();
      $('#rawfiles_tbl_allfiles tbody tr').removeClass('rawfiles_tbl_td_selected');
   });
   $('#btnResetExpStructCoord').on("click", function () {
      reset_reps();
      $("#btnResetExpStructCoord").prop('disabled', true);
      $("#s22btnf").prop('disabled', true);
   });
   // CSS
   $(".tooltip").hover(function () {
      $(".tooltip span").css({"margin-left": -$(".tooltip span").width() / 2 + 9});
      $(".callout").css({"left": $(".tooltip span").width() / 2});
   });
   // Bind test datasets dialog buttons
   $("#dlgTestDatasetsBtnOK").on("click", function () {
      dlgFadeout();
      postTestDatasetInfo($("#s1TestDatasetsSelection option:selected").text());
   });
   $("#dlgTestDatasetsBtnCancel").on("click", function () {
      dlgFadeout();
   });
   $("#dlgTestDatasetsBtnDownload").on("click", function () {
      dlgFadeout();
      var tdname = $("#s1TestDatasetsSelection option:selected").text();
      if(tdname){
         downloadTestDataset(tdname);
      }
   });
   // Initialize Raw file DataTable
   rawfiles_tbl_allfiles_DT = $('#rawfiles_tbl_allfiles').DataTable({
      paging: false,
      bInfo: false,
	  "pageLength": 308,
	  scrollY: "135px",
	  scrollCollapse: true,
	  
      "columnDefs": [
         {
            targets: [1, 2, 3],
            width: '5%',
            className: "dt-center"
         },
         {
            targets: 0,
            width: '80%',
            className: "dt-left",
			title: "Raw File"
         },
		 {
			 targets: 1,
			title: "B"
		 },
		 {
			 targets: 2,
			title: "T"
		 },
		 {
			 targets: 3,
			title: "F"
		 }
      ],
      "dom": '<"top ibvspace"f>rtT',
		"sDom": 'lfrtip',
		"oLanguage": {
         "sSearch": "Filter: "
      },
      "aoColumns": [
         {"mData": "fname"},
         {"mData": "brep"},
         {"mData": "trep"},
         {"mData": "frac"}
      ]
   });
   $('#rawfiles_tbl_allfiles').css({border: 'none'});
   $('#rawfiles_tbl_allfiles').css({margin: '0px'});
   $('#rawfiles_tbl_allfiles thead th').css({border: 'none'});
   $('#rawfiles_tbl_allfiles').css({'border-bottom': 'none'});
   $('#rawfiles_tbl_allfiles').css({'border-bottom': 'none'});
   $('#btnRawfilesTblSelectAll').on("click", function () {
      $('#rawfiles_tbl_allfiles tbody tr').addClass('rawfiles_tbl_td_selected');
   });
   $('#btnRawfilesTblSelectNone').on("click", function () {
      $('#rawfiles_tbl_allfiles tbody tr').removeClass('rawfiles_tbl_td_selected');
   });
   $('#btnRawfilesTblDelete').on("click", function () {
      if ($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected').length == 0) {
         return;
      }
      rawfiles_to_rem = $.map($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected'), function (val, i) {
         return $(val).prop('id');
      });
      rawfiles = $.grep(rawfiles, function (n, i) {
         return rawfiles_to_rem.indexOf('tr_' + n) == -1;
      });
      $("#s3expparamsDlgLabelsSelection .hidden").removeClass('hidden');
      $("#s3expparamsDlgLabelsSelection option").each(function (i) {
         if (rawfiles.indexOf($(this).val()) == -1) {
            $(this).addClass('hidden');
         }
      });
      if ($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected td:nth-child(2)').text().match(/[^-]+/g) != null) {
         rawfiles_tbl_allfiles_DT.rows($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected')).remove();
         reset_reps();
      } else {
         rawfiles_structure = $.grep(rawfiles_structure, function (n, i) {
            return rawfiles_to_rem.indexOf('tr_' + n.rawfile) == -1;
         });
         rawfiles_tbl_allfiles_DT.rows($('#rawfiles_tbl_allfiles .rawfiles_tbl_td_selected')).remove();
         rawfiles_tbl_allfiles_DT.draw();
      }
      $("#s22btnf").prop('disabled', !(rawfiles.length > 0 && rawfiles_structure.length == rawfiles.length));
      $("#btnResetExpStructCoord").prop('disabled', rawfiles_structure.length == 0);
   });
	
   // TEST DATA INIT
   postTestDatasetsInfo();
   //
   postClientServerClientInfo();
});





