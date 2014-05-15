var sessionid = new Date().getTime();
var clientname = '';
var softversion = '';
var cgi_bin_path = 'cgi-bin/';

// for counting the number of elements in an associative array
// from: http://stackoverflow.com/questions/5223/length-of-javascript-object-ie-associative-array
Object.size = function(obj) {
    var size = 0, key;
    for (key in obj) {
        if (obj.hasOwnProperty(key)) size++;
    }
    return size;
};

var inputChCheck = function(e,repatt,maxCharacters){
	var re = new RegExp(repatt);
	var srcelem = e.target || e.srcElement;
	var txt = srcelem.value + String.fromCharCode(e.which);
	if(!re.test(txt) || txt.length > maxCharacters){
		e.preventDefault();
	}
}

// from http://codereview.stackexchange.com/questions/7001/better-way-to-generate-all-combinations
var combinations = function(str) {
    var fn = function(active, rest, a) {
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

var combinations_arr = function(arr,delim){
	var ret = [];
	var str = "0123456789";
	var combs = combinations(str.slice(0,arr.length));
	for (var i in combs){
		var comb_i = combs[i];
		var ret_i = "";
		ret_i = arr[str.indexOf(comb_i[0])];
		for(var i=1;i<comb_i.length;i++){
			ret_i = ret_i + delim + arr[str.indexOf(comb_i[i])];
		}
		ret.push(ret_i);
	}
	return ret;
}


// From an array of items matched by "selector", choose the one with index "idx" and set some attribute(s)
var helper_setItemArrAttr = function(selector,idx,json){
	$($(selector).get(idx)).attr(json);
}

// For a single item matched by "selector" set some attribute(s)
var helper_setItemAttr = function(selector,json){
	$(selector).attr(json);
}

// Adds/Removes ("_removeClass" false/true) class "className" in succession (each time its called) for a list of items.
// Assumes ONLY ONE item in the list "itms" SHOULD/SHOULD NOT ("_removeClass" false/true) have the class "className".
// toggleFun executes code with the current (0-based) index as a parameter.
var toggleNextClass = function(itms, className, _removeClass, toggleFun){
	var nToggle = 0;
	var toggleClassFunction = (_removeClass ? ["removeClass","addClass"] : ["addClass","removeClass"]);
	var reachedLastItem = false;
	itms.some(function(itm, index){
		var cond = (_removeClass ? $(itm).hasClass(className) : !$(itm).hasClass(className));
		if(cond){
			if(nToggle > 0){
				$(itm)[toggleClassFunction[0]](className);
				nToggle++;
			}
		}else{
			if(toggleFun != null){
				if(toggleFun(index)){
					$(itm)[toggleClassFunction[1]](className);
					nToggle++;
				};
			}else{
				$(itm)[toggleClassFunction[1]](className);
				nToggle++;
			}			
		}
		return (nToggle === 2);
	});
	// Toggle 1st item again if toggled all in succession (valid when we have more than one items in the list)
	if(nToggle == 1 && itms.length > 1){
		var itm = itms[0];
		var cond = (_removeClass ? $(itm).hasClass(className) : !$(itm).hasClass(className));
		if(cond){
			$(itm)[toggleClassFunction[0]](className);
		}else{
			$(itm)[toggleClassFunction[1]](className);
		}
	}
}

// Get array of items from the DOM using a selector and id pattern
var getItems = function(className, id_pattern){
	return $(className).toArray().filter(function(element){
		var match = ($(element).attr("id")).match(id_pattern);
		return (match == null ? false : true);
	});
}

// Get an array of items IDs' strings (starting with a #)
var idsToStrings = function(itemsSelector){
	return $.map($(itemsSelector).toArray(), function(itm, i){
				return "#" + $(itm).attr("id");
	});
}

// Called by "toggleNextClass". It is used for hiding the "Previous" button when we are at stage 1, i.e. there is no previous stage.
var nextStageIsFirstStage = function(lastStageItem){
	$("#s1btnb").addClass("hidden");
}

// Reset styles based on source CSS (i.e. discard changes made through JS)
var resetCssStyles = function(selector){
	$(selector).removeAttr("style");
}

// Self-explanatory
var setItemAttrColor = function(selector,attr,hexcolor){
	var cssstr = $(selector).css(attr);
	$(selector).css(attr,cssstr.replace(/rgb\([0-9]+, [0-9]+, [0-9]+\)$/g,hexcolor));
}

// Clear areas where results information appears.
var resetResultsInfoItms = function(){
	$("#server_feedback").empty();
	$("#dndres").empty();
	$("#results_p").html("Plase wait (up to 5 minutes) for your results.");
}

var postFireUpAnalysisAndWait = function(){
	var thedata = new FormData();
	thedata.append('session_id', sessionid);
    $.ajax({
        url: cgi_bin_path + 'perform_analysis.php',  //Server script to fire up data analysis
        type: 'POST',
        // Form data
        data: thedata,
        //Options to tell jQuery not to worry about content-type.
		processData: false,
        cache: false,
		contentType: false,
        beforeSend: function(jqXHR, settings){
			//fire-up spinner
			$("#server_feedback").html("<div class='loadingclockcontainer'><div class='box'><div class='clock'></div></div></div>");
		},		
        success: function(data, textStatus, jqXHR){
			$("#server_feedback").empty();
			$("#s4btnf").prop('disabled', !data.success);
			if(data.success){
				$("#results_p").html("Now you can inspect your results. When ready, click <em>Next</em>.");
				$("#dndres").html("<span><a href="+ data.results_url +"><strong>" + data.results_url.substr(data.results_url.lastIndexOf("/") + 1) + "</strong></a></span>");
				patt = new RegExp("limma\-graphs");
				$.each(data.results_preview, function(idx, path_to_img_i)
				{
					var img_i = path_to_img_i.substr(data.results_url.lastIndexOf("/") + 1);
					if(!patt.test(img_i)){
						$("#server_feedback").append("<div class='resimg'><a href='"+ path_to_img_i +"' target='_blank'><img src='"+ path_to_img_i +"' width='120'></img></a></div>");
					}
				});
			}else{
				$("#results_p").html("");
				$("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>The analysis could not be completed: " + data.msg + "<em><strong></span>");
				if(data.R_dump.length > 0){
					$("#server_feedback").append("<br><br><span style='font-family: Georgia; font-size: 95%; text-align: left; display: inline-block; width: 90%'><p>Please ensure that input parameters (such as number of replicates, data format, number of biological conditions/labels etc) are correctly defined. The statistical analysis routine relies heavily on the validity of the input parameters, i.e. minor deviations from the correct parameter values may bring the analysis to halt.</p><p>If the above does not apply, then the statistical analysis may have been halted due to numerical problems (e.g. there were too many missing values/data points).</p><p> If you feel that none of the above is relevant, please click <a href='mailto:msdiffexp@gmail.com?Subject=Session%20"+sessionid+"' target='_blank'><u>here</u></a> to notify via e-mail (please do delete the number in the subject) the ProteoSign team for further investigation of your analysis issue.</p></span>")
					//$("#server_feedback").append("<br><br><span style='font-family: \"Courier New\"'>Logged information:</span><br>");
					//$("#server_feedback").append("<div style='height: inherit'><textarea style='font-family: \"Courier New\"; font-size: 90%; width: 90%; height: 100%' readonly>"+ data.dump +"</textarea></div>");
					debugger;
				}
			}		
		},
        error: function(jqXHR, textStatus, errorThrown){
			$("#server_feedback").empty();
			$("#server_feedback").html("<span class='uploadErrorMsg'><strong><em>An AJAX error occurred: " + errorThrown + "<em><strong></span>");
		}	
    });	
}

var postClientServerClientInfo = function(){
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
        beforeSend: function(jqXHR, settings){},
        success: function(data, textStatus, jqXHR){
			clientname = data.hostname;
			softversion = data.version;
			$("#scrollingtext").html("Welcome <em>"+clientname+"</em> to ProteoSign");
			$("#proteosignversion").html("ProteoSign version "+softversion);
		},
        error: function(jqXHR, textStatus, errorThrown){}
    });	
}

var postParameters = function(params) {
	var thedata = new FormData();
	thedata.append('session_id', sessionid);
	$.each(params, function(idx, param_i)
	{
		switch($(param_i).attr('type')){
			case "checkbox":
				// Here the on/off is transmitted Yes/No
				var theval = ($(param_i).prop("checked") ? "Yes" : "No");
				thedata.append($(param_i).attr('name'), theval);
				break;
			default:
				var theval = $(param_i).val();
				if(theval == null){
					theval = '';
				}
				thedata.append($(param_i).attr('name'), theval);
				break;
		}
		//console.log($(param_i).attr('name')+" = "+theval);
	});
	//final parameter
	thedata.append("labelfree", ((peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0) ? 'Yes' : 'No'));
    $.ajax({
        url: cgi_bin_path + 'upload_parameters.php',  //Server script to receive parameters
        type: 'POST',
        // Form data
        data: thedata,
        //Options to tell jQuery not to worry about content-type.
		processData: false,
        cache: false,
		contentType: false,
        success: function(data, textStatus, jqXHR){
			//if there was a server-side error alert.
			if(!data.success){
				alert("ERROR on SERVER: " + data.msg);
			}else{
				postFireUpAnalysisAndWait();
			}
		}	
    });
}

// Executed when a "Next" type of button is clicked
var executeStage = function(stageIndex){
	var ret = true;
	switch(stageIndex){
		case 1:
			break;
		case 2:
			var parameterObjs = $("#s3expparams input,#s3expparams select");
			if(validateParameters(parameterObjs)){
				postParameters(parameterObjs);
				// Clear areas where results information appears.
				resetResultsInfoItms();
			}else{
				ret = false;
			}
			break;
		default:
	}
	return ret;
}
// Executed when a "Back" type of button is clicked
var rollbackStage = function(stageIndex){
	var ret = true;
	return ret;
}

var unsuccessfullyUploadedFiles = {};
var uploadEfforts = {};
var peptideLabelsFromFile = [];
var peptideLabelsNamesFromFile = [];
var peptideLabelsFromFileCombs = [];
var nFormLabels = 1;
var nToUpload;

// Return false if at least one required parameter (existence of data-required attr) is not set (empty text field)
// Mark invalid parameters with a red border color (currently tested on input fields)
// Marked fields are reset through the 'focusout' event (see respective binding in function document.ready)
var validateParameters = function(params) {
	var nValid = 0;
	var nInvalid = 0;
	$.each(params, function(idx, param_i)
	{
		switch($(param_i).attr('type')){
			case "text":
				if($(param_i).attr('data-required') != null && $(param_i).attr('data-required')){
					if($(param_i).val() == "" && !$(param_i).hasClass("hidden")){
						nInvalid++;
						setItemAttrColor(param_i,"border","#E60000");
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

// "uploadingFiles": Files (array returned by FileChooser) selected for upload in stage #2
var uploadFiles = function(uploadingFiles){
	// reset "counters"/states
	unsuccessfullyUploadedFiles = {};
	uploadEfforts = {};
	peptideLabelsFromFile = [];
	peptideLabelsNamesFromFile = [];
	peptideLabelsFromFileCombs = [];
	// Reset items that contained information from previous data input files (e.g. label information)
	$("#s3expparamsDlgLabelsSelection").empty();
	$("#explbl1name_").empty();
	$("#explbl0name_").empty();
	$("#s3advparams select[name='expquantfiltlbl']").empty();
	$("#quantsoftdetectedspan").empty();
	while(nFormLabels > 1){
		removeFormLabel();
	}	
	if(!$("#explbl1definition").hasClass("hidden")){
		$("#explbl1definition").addClass("hidden");
	}
	//
	nToUpload = uploadingFiles.length;
	$("#s2btnf").prop('disabled', true);
	// fire AJAX calls
	$.each(uploadingFiles, function(idx, file_i)
	{
		postFile(idx,file_i);
	});
}

// Called by respective AJAX event handlers (see postFile function below)
var uploadFinished = function(success,idx,file){
	uploadEfforts[idx] = file;
	if(!success){
		unsuccessfullyUploadedFiles[idx] = file;
	}
}

// jQuery AJAX POST for uploading a single file
var postFile = function(idx,file) {
	var thedata = new FormData();
	thedata.append('thefile', file);
	thedata.append('cleanprev', idx == (nToUpload-1));
	thedata.append('session_id', sessionid);
	var progresstditm =  $("#s2uluploaders table tr:nth-child("+(idx+1)+") td").get(1);
    $.ajax({
        url: cgi_bin_path + 'upload_files.php',  //Server script to receive file
        type: 'POST',
        xhr: function() {  // Custom XMLHttpRequest
            var myXhr = $.ajaxSettings.xhr();
            if(myXhr.upload){ // Check if upload property exists
                myXhr.upload.addEventListener('progress',function(e){
					if(e.lengthComputable){
						helper_setItemAttr("#uploadfile" + idx,{value: e.loaded,max: e.total});
					}				
				}, false); // For handling the progress of the upload
            }
            return myXhr;
        },
        //Ajax events
        beforeSend: function(jqXHR, settings){
			
		},
        success: function(data, textStatus, jqXHR){
			//remove progress bar
			$(progresstditm).empty();
			//If server-side everything went fine (internal things that the server had to do with the client's file, such as storage etc)
			uploadFinished(data.success, idx, file);
			//if everything went fine enable button for next stage and print OK. Just print the error message otherwise.
			if(data.success){
				$(progresstditm).html("<span class='uploadSuccessMsg'><strong><em>OK<em><strong></span>");
				// If we have info regarding peptide labels and such info has not been available in previous upload
				if(peptideLabelsNamesFromFile.length == 0 && (data.peptide_labels_names.length > 0 || data.peptide_labels.length > 0)){
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
					if(peptideLabelsNamesFromFile.length > 0){
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
					}else{
						//label-free case (disable non-applicable parameters and rename others accordingly)
						$(item1).prop('disabled', true);
						$(item2).prop('disabled', true);
						$(item3).html('&#8212 Biological condition #1');
						$(item4).html('<input data-required="true" id="explbl1name_" name="explbl1name" type="text" onkeypress="inputChCheck(event,\'^(?!_)[a-zA-Z0-9_]+$\',20)" placeholder="Character rules apply"></input>');
						$(item5).addClass('hidden');
						$(item6).attr('placeholder', 'Raw file');
					}
					
					$.each(peptideLabelsFromFile,function(idx, lbl_i){
						$("#s3expparamsDlgLabelsSelection").append("<option value='"+lbl_i+"'>"+lbl_i+"</option>");
					});
					$.each(peptideLabelsNamesFromFile,function(idx, lblname_i){
						$("#explbl1name_").append("<option value='"+lblname_i+"'>"+lblname_i+"</option>");
						$("#explbl0name_").append("<option value='"+lblname_i+"'>"+lblname_i+"</option>");
						$("#s3advparams select[name='expquantfiltlbl']").append("<option value='"+lblname_i+"'>"+lblname_i+"</option>");
					});
					if(peptideLabelsFromFile.length > 0){
						$("#explbl1definition").removeClass("hidden");
					}
					$("#s2btnf").prop('disabled', false);
				}
			}else{
				$(progresstditm).html("<span class='uploadErrorMsg'><strong><em>A server-side error occurred: " + data.msg + "<em><strong></span>");
			}
		},
        error: function(jqXHR, textStatus, errorThrown){
			$(progresstditm).empty();
			$(progresstditm).html("<span class='uploadErrorMsg'><strong><em>An AJAX error occurred: "+errorThrown+"<em><strong></span>");
			uploadFinished(false, idx, file);
		},
        // Form data
        data: thedata,
        //Options to tell jQuery not to process data or worry about content-type.
        cache: false,
        contentType: false,
        processData: false
    });  
}

var bind_explbldefinition_focus = function(explbldefinition){
	$(explbldefinition).on("focus",function(){
		$(".expparamsDlg").css({"left" : ($("body").width()/2) - ($("#s3expparamsDlgLabels").width()/2)});
		$('body').append('<div id="mask"></div>');
		$("#s3expparamsDlgLabels").fadeIn(300);
		$('#mask').fadeIn(300);
		var lbl = $(this);
		$("#dlglabelsBtnOK").unbind();
		$("#dlglabelsBtnOK").on("click",function(){
			$(".expparamsDlg").fadeOut(300 , function() {
				$('#mask').remove();  
			});
			var selection = $("#s3expparamsDlgLabelsSelection").val();
			if(selection != null){
				$(lbl).val(selection.join(","));
			}
		});
	});			
}

var addFormLabel = function(){
	nFormLabels++;
	var last_tr_of_table = $("#btnAddLabel").closest("table").children().first().children().last();
	if(peptideLabelsNamesFromFile.length > 0){
		$(last_tr_of_table).after("<tr><td>&#8212 Species label #"+nFormLabels+"</td><td></td><td><select data-required='true' id='explbl"+nFormLabels+"name_' name='explbl"+nFormLabels+"name'></select></td><td><input data-required='true' id='explbl"+nFormLabels+"definition' name='explbl"+nFormLabels+"def' type='text' placeholder='Definition' readonly"+(peptideLabelsFromFile.length > 0 ? "" : " class='hidden'")+"/></td></tr>");
		if(peptideLabelsFromFile.length > 0){
			bind_explbldefinition_focus("#explbl"+nFormLabels+"definition");
			$("#explbl"+(nFormLabels-1)+"definition").off("focus");
		}
		$.each(peptideLabelsNamesFromFile,function(idx, lblname_i){
			$("#explbl"+nFormLabels+"name_").append("<option value='"+lblname_i+"'>"+lblname_i+"</option>");
		});
	}else{
		//label-free case
		if(peptideLabelsFromFile.length > 0){
			$(last_tr_of_table).after("<tr><td>&#8212 Biological condition #"+nFormLabels+"</td><td></td><td><input data-required='true' id='explbl"+nFormLabels+"name_' name='explbl"+nFormLabels+"name' type='text' onkeypress='inputChCheck(event,\"^(?!_)[a-zA-Z0-9_]+$\",20)' placeholder='Character rules apply'></input></td><td><input data-required='true' id='explbl"+nFormLabels+"definition' name='explbl"+nFormLabels+"def' type='text' placeholder='Raw file' readonly"+(peptideLabelsFromFile.length > 0 ? "" : " class='hidden'")+"/></td></tr>");
			bind_explbldefinition_focus("#explbl"+nFormLabels+"definition");
			$("#explbl"+(nFormLabels-1)+"definition").off("focus");			
		}
	}
	var addbtntd = $("#btnAddLabel").closest("td");
	var rembtntd = $(addbtntd).next();
	last_tr_of_table = $(last_tr_of_table).next();
	$(last_tr_of_table).children().last().after($(addbtntd));
	$(last_tr_of_table).children().last().after($(rembtntd));
	$("#btnRemLabel").prop("disabled", false);
	// label-free case
	if(peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0){
		$("#btnAddLabel").prop("disabled",nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsFromFile.length - 2) : peptideLabelsFromFile.length - 1));
	}else{
		$("#btnAddLabel").prop("disabled",nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsNamesFromFile.length - 2) : peptideLabelsNamesFromFile.length - 1));
	}
}

var removeFormLabel = function(){
	nFormLabels--;
	var rembtntd = $("#btnRemLabel").closest("td");
	var addbtntd = $(rembtntd).prev();
	var last_tr_of_table = $("#btnRemLabel").closest("table").children().first().children().last();
	$(last_tr_of_table).prev().children().last().after($(addbtntd));
	$(last_tr_of_table).prev().children().last().after($(rembtntd));
	$(last_tr_of_table).remove();
	$("#btnRemLabel").prop("disabled",nFormLabels == 1);
	// label-free case
	if(peptideLabelsNamesFromFile.length == 0 && peptideLabelsFromFile.length > 0){
		$("#btnAddLabel").prop("disabled",nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsFromFile.length - 2) : peptideLabelsFromFile.length - 1));
	}else{
		$("#btnAddLabel").prop("disabled",nFormLabels > ($("#s3advparams input[name='explbl00']").prop('checked') ? (peptideLabelsNamesFromFile.length - 2) : peptideLabelsNamesFromFile.length - 1));
	}
	if(peptideLabelsFromFile.length > 0){
		bind_explbldefinition_focus("#explbl"+nFormLabels+"definition");
	}
}

$(document).ready(function() {
	var forward_buttons = getItems("button.main", /s[0-9]+btnf/);
	var backward_buttons = getItems("button.main", /s[0-9]+btnb/);
	// Binds the click event to "toggleNextClass" for each "forward button" (button with class button.main and id /s[0-9]+btnf/) 
	forward_buttons.forEach(function(btn){
		$(btn).on("click",function(){
			toggleNextClass(idsToStrings(".main_div .main_section"),"hidden", true, executeStage);
			if($(backward_buttons[0]).hasClass("hidden")){
				$(backward_buttons[0]).removeClass("hidden");
			}
		});
	});
	// Binds the click event to "toggleNextClass" for each "backward button" (button with class button.main and id /s[0-9]+btnb/) 
	backward_buttons.forEach(function(btn){
		$(btn).on("click",function(){
			toggleNextClass(idsToStrings(".main_div .main_section").reverse(),"hidden", true, rollbackStage);
		});
	});
	// Bind event for file upload button
	$("#s2btnupld").on("click",function(){
		$("#__s2btnupld").click();
	});
	// Bind event when file(s) is/are chosen
	$("#__s2btnupld").change(function(){
		var fnames = "";
		$("#s2uluploaders > table").empty();
		var uploadingFiles = this.files;
		$.each(uploadingFiles,function(idx, file_i){
			$("#s2uluploaders table").append("<tr><td>" + file_i.name + "</td><td><progress max='100' value='0' id=uploadfile" + idx + "><div class='progress-bar'><span style='width: 80%;'></span></div></progress></td></tr>");
		});
		//Start uploading ...
		uploadFiles(uploadingFiles);
	});
	//Toggle visibility of table with advanced params
	$("#s3showhideadvparams").on("click",function(){
		var txt = $(this).text();
		if($("#s3advparams").hasClass("hidden")){
			$("#s3advparams").removeClass("hidden");
			$(this).text(txt.replace("Show","Hide"));
			var parentdiv = $(this).closest("div");
			parentdiv.scrollTop(parentdiv.prop("scrollHeight"));			
		}else{
			$("#s3advparams").addClass("hidden");
			$(this).text(txt.replace("Hide","Show"));
		}
	});
	//Add functionality of add/remove labels buttons
	$("#btnAddLabel").on("click",addFormLabel);
	$("#btnRemLabel").on("click",removeFormLabel);
	bind_explbldefinition_focus("#explbl"+nFormLabels+"definition");
	$("#dlglabelsBtnCancel").on("click",function(){
		$(".expparamsDlg").fadeOut(300 , function() {
			$('#mask').remove();  
		}); 
	});
	//Toggle visibility on inputs that are dependent to each other
	// #1: dependency between quantitation filtering settings
	$("#s3advparams input[name='expquantfilt']").on("click", function(){
		if($(this).prop('checked')){
			$("#s3advparams select[name='expquantfiltlbl']").closest("tr").removeClass("hidden");
			$("#s3advparams input[name='expquantfiltprot']").closest("tr").removeClass("hidden");
			var parentdiv = $(this).closest("div");
			parentdiv.scrollTop(parentdiv.prop("scrollHeight"));
		}else{
			$("#s3advparams select[name='expquantfiltlbl']").closest("tr").addClass("hidden");
			$("#s3advparams input[name='expquantfiltprot']").closest("tr").addClass("hidden");
		};
	});
	
	// #2: dependency between PD data and label definitions.
	$("#s3expparams input[name='exppddata']").on("click", function(){
	});
	//TODO
	// #3: dependency between background existence and ?
	$("#s3advparams input[name='explbl00']").on("click", function(){
		if($(this).prop('checked')){
			$("#s3advparams select[name='explbl0']").closest("tr").removeClass("hidden");
			if(nFormLabels > peptideLabelsNamesFromFile.length - 1){
				removeFormLabel();
			}
			var parentdiv = $(this).closest("div");
			parentdiv.scrollTop(parentdiv.prop("scrollHeight"));	
		}else{
			$("#s3expparams select[name='explbl0']").closest("tr").addClass("hidden");
		}
	});	
	// TextField validation
	$("#s3expparams input[name='expbioreps'],#s3expparams input[name='exptechreps']").on("focusout",function(){
		if($(this).val() != "" && $(this).val()<2){
			$(this).focus().select();
		}
	});
	// Needed after form validation (because some required fields might have been highlighted)
	$("#s3expparams input[data-required]").on("focusout",function(){
		resetCssStyles(this);
	});

	// CSS
	$(".tooltip").hover(function(){
		$(".tooltip span").css({"margin-left":-$(".tooltip span").width()/2+9});
		$(".callout").css({"left":$(".tooltip span").width()/2});
		//$(".callout").css({"top":$(this).position.top});
	});
	//
	postClientServerClientInfo();
});


	



